structure Compile :
sig
  datatype err =
    Compilation of string * PolyML.location
  | Dependency  of string (* depsFirst invariant violated; means bad input *)
  | Execution   of string * exn
  | UnboundId   of string

  exception Compile of err

  type opts =
    { depsFirst : bool
    , jobs      : int
    , logger    : Log.logger option
    }

  (* Resolve and compile a list of declarations in a fresh env, triggering
   * side effects from top level declarations.
   * Does not handle IO.Io and raises Compile on non IO error.
   * single job and depsFirst = false is guaranteed to be encounter order.
   * The logger may be called from different threads if multiple jobs.
   *)
  val compile : opts -> Dag.t -> NameSpace.t
end =
struct
  structure A    = Array
  structure BA   = BoolArray
  structure D    = Dag
  structure FTP  = ThreadPool.FTP
  structure H    = HashArray
  structure L    = List
  structure M    = Thread.Mutex
  structure NS   = NameSpace
  structure P    = PolyML
  structure PC   = PolyML.Compiler
  structure PTP  = ThreadPool.PTP
  structure TIO  = TextIO
  structure TSIO = TIO.StreamIO
  structure V    = Vector

  datatype err =
    Compilation of string * P.location
  | Dependency  of string
  | Execution   of string * exn
  | UnboundId   of string

  exception Compile of err

  type opts =
    { depsFirst : bool
    , jobs      : int
    , logger    : Log.logger option
    }

  datatype 'a r = Done of 'a * NS.t | Cont of 'a * string * 'a cont
  withtype 'a cont = NS.t -> 'a r

  fun compileSML log (ns, path, opts) =
    let
      val msg = ref ([] : string list)
      val loc = ref
        { file = ""
        , startLine = 0, startPosition = 0
        , endLine = 0, endPosition = 0
        }

      fun msgCb { message, hard, location, ... } =
        if hard then
          ( P.prettyPrint (fn s => msg := s :: !msg, 80) message
          ; loc := location
          )
        else
          case log of
            NONE => ()
          | SOME { pathFmt, print } =>
              let
                val m = ref ([] : string list)
              in
                P.prettyPrint (fn s => m := s :: !m, 80) message;
                print
                  ( Log.Warn
                  , fn () => String.concat
                      (Log.locFmt pathFmt location :: ": " :: List.rev (!m))
                  )
              end

      val s = (ref o TIO.getInstream o TIO.openIn) path
      val l = ref 1

      fun getc () =
        case TSIO.input1 (!s) of
          NONE => NONE
        | SOME (c as #"\n", s') => (l := !l + 1; s := s'; SOME c)
        | SOME (c, s') => (s := s'; SOME c)

      val opts =
        [ PC.CPErrorMessageProc msgCb
        , PC.CPLineNo (fn () => !l)
        , PC.CPFileName path
        , PC.CPNameSpace ns
        ] @ opts
    in
      while (not o TSIO.endOfStream o !) s do
        let
          val f = P.compiler (getc, opts)
            handle _ =>
              ( TSIO.closeIn (!s)
              ; raise
                  (Compile o Compilation)
                  ((String.concat o List.rev o !) msg, !loc)
              )
        in
          f ()
            handle e =>
              ( TSIO.closeIn (!s)
              ; raise (Compile o Execution) (path, e)
              )
        end;
      TSIO.closeIn (!s)
    end

  fun anns l =
    let
      fun f (Ann.Debug b, opts) = PC.CPDebug b :: opts
        | f (_, opts) = opts
    in
      List.foldr f [] l
    end

  fun isIgnored (l, p) =
    let
      val p = OS.Path.file p
    in
      List.exists (fn p' => p' = p) l
    end

  local
    datatype z = datatype Basis.dec
    datatype z = datatype Basis.exp
  in
    fun compileBas log ns ret ds =
      let
        fun elab (opts, ign) (ns as NS.N (bns, pns), ds, cont) =
          let
            val elab' = elab (opts, ign)

            fun dec ds =
              case (Thread.Thread.testInterrupt (); ds) of
                [] => cont ns
              | Basis (b, e) :: ds =>
                  exp (e, ns, fn ns' => (#enterBas bns (b, ns'); dec ds))
              | BasisFile p :: ds =>
                if isIgnored (ign, p) then
                  dec ds
                else
                 Cont (ret, p, fn ns' => (NS.import (ns, ns'); dec ds))
              | SourceFile p :: ds =>
                  if isIgnored (ign, p) then
                    dec ds
                  else
                    ( case log of
                        NONE => ()
                      | SOME { pathFmt, print } =>
                          print (Log.Debug, fn () => "compiling " ^ pathFmt p)
                    ; compileSML log (pns, p, opts)
                    ; dec ds
                    )
              | Ann (l, ds') :: ds =>
                  if Ann.exists Ann.Discard l then
                    dec ds
                  else
                    let
                      val ign' =
                        List.foldl
                          (fn (Ann.IgnoreFiles f, fs) => f @ fs | (_, fs) => fs)
                          ign l

                      val ns' =
                        if Ann.exists Ann.ImportAll l then
                          let
                            val { loc, pub } = NS.delegates ns
                          in
                            NS.import (loc, NS.all);
                            pub
                          end
                        else
                          ns
                    in
                      elab (anns l @ opts, ign') (ns', ds', fn _ => dec ds)
                    end
              | Local (ds1, ds2) :: ds =>
                  let
                    val { loc, pub } = NS.delegates ns
                  in
                    elab' (loc, ds1, fn _ => elab' (pub, ds2, fn _ => dec ds))
                  end
              | Open b :: ds =>
                  ( case #lookupBas bns b of
                      NONE => raise Compile (UnboundId b)
                    | SOME ns' => NS.import (ns, ns')
                  ; dec ds
                  )
              | Structure (new, old) :: ds =>
                  ( case #lookupStruct pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME s => #enterStruct pns (new, s)
                  ; dec ds
                  )
              | Signature (new, old) :: ds =>
                  ( case #lookupSig pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME s => #enterSig pns (new, s)
                  ; dec ds
                  )
              | Functor (new, old) :: ds =>
                  ( case #lookupFunct pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME f => #enterFunct pns (new, f)
                  ; dec ds
                  )

            and exp (e, ns as NS.N (bns, _), cont) =
              case e of
                Bas ds =>
                  let
                    val ns' = NS.empty ()
                    val { loc, pub } = NS.delegates ns'
                  in
                    NS.import (loc, ns);
                    elab' (pub, ds, fn _ => cont ns')
                  end
             | Id b =>
                  (case #lookupBas bns b of
                    NONE => raise Compile (UnboundId b)
                  | SOME ns => cont ns)
              | Let (ds, e) =>
                  let
                    val ns' = NS.empty ()
                    val { loc, pub } = NS.delegates ns'
                  in
                    NS.import (loc, ns);
                    elab' (loc, ds, fn _ => exp (e, pub, fn _ => cont ns'))
                  end
          in
            dec ds
          end
      in
        elab ([], []) (getOpt (ns, NS.empty ()), ds, fn ns => Done (ret, ns))
      end
  end

  fun logElab NONE _ = ()
    | logElab (SOME { pathFmt, print }) p =
        print (Log.Info, fn () => "elaborating " ^ pathFmt p)

  fun serialDeps log ({ root as D.N (r, _), bases, paths, id, ... } : D.t) =
    let
      val nss : NS.t option array = A.array (V.length bases, NONE)

      fun cont (Done _) = ()
        | cont (Cont (_, p, f)) =
            case A.sub (nss, id p) of
              SOME ns => cont (f ns)
            | NONE => raise Compile (Dependency p)

      fun f (D.N (p, deps)) =
        case A.sub (nss, p) of
          SOME _ => ()
        | NONE =>
            let
              val ns = NS.empty ()
            in
              A.update (nss, p, SOME ns);
              V.app f deps;
              (logElab log o V.sub) (paths, p);
              (cont o compileBas log (SOME ns) p o V.sub) (bases, p);
              ()
            end
    in
      f root;
      (valOf o A.sub) (nss, r)
    end

  fun serialEncounter log ({ root = D.N (r,  _), bases, paths, id, ... } : D.t) =
    let
      val nss : NS.t option array = A.array (V.length bases, NONE)

      fun cont (Done (p, ns)) = ns before A.update (nss, p, SOME ns)
        | cont (Cont (_, p, f)) =
            let
              val i = id p
            in
              case A.sub (nss, i) of
                SOME ns => cont (f ns)
              | NONE =>
                  let
                    val _ = logElab log p
                    val ns = (cont o compileBas log NONE i o V.sub) (bases, i)
                  in
                    cont (f ns)
                  end
            end
    in
      (logElab log o V.sub) (paths, r);
      (cont o compileBas log NONE r o V.sub) (bases, r)
    end

  fun parDeps jobs log { root as D.N (s, _), leaves, bases, paths, id } =
    let
      val counts = A.tabulate (V.length bases, fn _ => (M.mutex (), ref ~1))
      val nss = A.tabulate (V.length bases, fn _ => NS.empty ())
      val tp = FTP.new jobs

       fun doCounts (D.N (s, v)) =
        let
          val (_, r) = A.sub (counts, s)
        in
          if !r > ~1 then
            ()
          else
            r := V.length v;
            V.app doCounts v
        end

      fun cont (Cont (_, p, f)) = (cont o f o A.sub) (nss, id p)
        | cont _ = ()

      fun postDep (n as D.N (s, _)) =
        let
          val (m, r) = A.sub (counts, s)
        in
          if !r <= 0 then
            FTP.submit (tp, comp n)
          else
            ( M.lock m
            ; r := !r - 1
            ; if !r = 0 before M.unlock m then
                FTP.submit (tp, comp n)
              else
                ()
            )
        end

      and comp (D.N (s, revs)) () =
        ( (logElab log o V.sub) (paths, s)
        ; (cont o compileBas log ((SOME o A.sub) (nss, s)) s o V.sub) (bases, s)
        ; V.app postDep revs
        )
    in
      doCounts root;
      V.app (fn n => comp n ()) leaves;
      case FTP.wait tp of
        NONE => A.sub (nss, s)
      | SOME e => raise e
    end

  fun parConc jobs log { root as D.N (s, _), leaves, bases, paths, id } =
    let
      type c = int * (int * int) cont

      val sz    = V.length bases
      val m     = M.mutex ()
      val dones = BA.array (sz, false)
      val conts = A.tabulate (sz, fn _ => ([] : c list))
      val nss   = V.tabulate (sz, fn _ => NS.empty ())
      val prios = A.array (sz, ~1)

      val tp = PTP.new jobs

      fun doPrio i (D.N (s, deps)) =
        let
          val j = A.sub (prios, s)
        in
            if j < i then
              ( A.update (prios, s, i)
              ; if j = ~1 then V.app (doPrio (i + 1)) deps else ()
              )
            else
              ()
        end

      fun cont (Done ((_, p), ns)) =
            ( M.lock m
            ; BA.update (dones, p, true)
            ; M.unlock m
            ; L.app
                (fn (p, f) => PTP.submit (tp, (p, fn () => cont (f ns))))
                (A.sub (conts, p))
            )
        | cont (Cont ((prio, _), p, f)) =
            let
              val i = id p
            in
              if BA.sub (dones, i) then
                (cont o f o V.sub) (nss, i)
              else
                if (M.lock m; BA.sub (dones, i)) then
                  (M.unlock m; (cont o f o V.sub) (nss, i))
                else
                  ( A.update (conts, i, (prio, f) :: A.sub (conts, i))
                  ; M.unlock m
                  )
            end

      fun comp (D.N (s, revs)) =
        let
          val p = A.sub (prios, s)
        in
          if p = ~1 then
            ()
          else
            ( A.update (prios, s, ~1)
            ; PTP.submit
                (tp, (p, fn () =>
                  ( (logElab log o V.sub) (paths, s)
                  ; ( cont
                    o compileBas log ((SOME o V.sub) (nss, s)) (p, s)
                    o V.sub
                    ) (bases, s)
                  )))
            ; V.app comp revs
            )
        end
    in
      doPrio 0 root;
      V.app comp leaves;
      case PTP.wait tp of
        NONE => V.sub (nss, s)
      | SOME e => raise e
    end

  fun numJobs j =
    if j <= 0 then
      Thread.Thread.numProcessors ()
    else
      Int.min (j, Thread.Thread.numProcessors ())

  fun compile { depsFirst, jobs, logger } =
    (case (numJobs jobs, depsFirst) of
      (1, true) => serialDeps
    | (1, _)    => serialEncounter
    | (n, true) => parDeps n
    | (n, _)    => parConc n) logger
end
