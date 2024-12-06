structure Compile :
sig
  datatype err =
    Compilation of string * PolyML.location
  | Dependency  of string (* depsFirst invariant violated; means bad input *)
  | Execution   of string * exn
  | UnboundId   of string
  | NoSuchMLB   of string

  exception Compile of err

  type opts =
    { copts     : PolyML.Compiler.compilerParameters list
    , depsFirst : bool
    , jobs      : int
    }

  (* Resolve and compile a list of declarations in a fresh env, triggering
   * side effects from top level declarations.
   * Does not handle IO.Io and raises Compile on non IO error.
   * single job and depsFirst = false is guaranteed to be encounter order.
   * The logger may be called from different threads if multiple jobs.
   *)
  val compile : Log.logger -> opts -> Dag.t -> NameSpace.t
end =
struct
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

  datatype err =
    Compilation of string * P.location
  | Dependency  of string
  | Execution   of string * exn
  | UnboundId   of string
  | NoSuchMLB   of string

  exception Compile of err

  type opts =
    { copts     : PolyML.Compiler.compilerParameters list
    , depsFirst : bool
    , jobs      : int
    }

  datatype 'a r = Done of 'a * NS.t | Cont of 'a * string * 'a cont
  withtype 'a cont = NS.t -> 'a r

  fun compileSML { pathFmt, print } (ns, path, opts) =
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
          let
            val m = ref ([] : string list)
          in
            P.prettyPrint (fn s => m := s :: !m, 80) message;
            print (Log.Warn,
                Log.locFmt pathFmt location ^ ": "
              ^ (String.concat o List.rev o !) m)
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
    fun compileBas (log as { pathFmt, print }) opts ns ret ds =
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
                    ( print (Log.Debug, "compiling " ^ pathFmt p)
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
        elab (opts, []) (getOpt (ns, NS.empty ()), ds, fn ns => Done (ret, ns))
      end
  end

  fun logElab { pathFmt, print } p =
    print (Log.Info, "elaborating " ^ pathFmt p)

  fun serialDeps (log, copts) ({ root as D.N (r, _, _), order, ... } : D.t) =
    let
      val nss : NS.t H.hash = H.hash (order * 5 div 4)

      fun cont (Done _) = ()
        | cont (Cont (_, p, f)) =
            case H.sub (nss, p) of
              SOME ns => cont (f ns)
            | NONE => raise Compile (Dependency p)

      fun f (D.N (p, bas, deps)) =
        case H.sub (nss, p) of
          SOME _ => ()
        | NONE =>
            let
              val ns = NS.empty ()
            in
              H.update (nss, p, ns);
              app f deps;
              logElab log p;
              cont (compileBas log copts (SOME ns) p bas);
              ()
            end
    in
      f root;
      (valOf o H.sub) (nss, r)
    end

  fun serialEncounter (log, copts) ({ root = D.N (r, b, _), bas, order, ... } : D.t) =
    let
      val nss : NS.t H.hash = H.hash (order * 5 div 4)

      fun cont (Done (p, ns)) = ns before H.update (nss, p, ns)
        | cont (Cont (_, p, f)) =
            case H.sub (nss, p) of
              SOME ns => cont (f ns)
            | NONE =>
                let
                  val _ = logElab log p
                  val ns = (cont o compileBas log copts NONE p o bas) p
                in
                  H.update (nss, p, ns);
                  cont (f ns)
                end
    in
      logElab log r;
      cont (compileBas log copts NONE r b)
    end

  fun parDeps jobs (log, copts) ({ root as D.N (s, _, _), leaves, order, ... } : D.t) =
    let
      val counts : (M.mutex * int ref) H.hash = H.hash 10
      val nss : NS.t H.hash = H.hash (order * 5 div 4)
      val tp = FTP.new jobs

      fun ns s =
        case H.sub (nss, s) of
          SOME _ => ()
        | NONE => H.update (nss, s, NS.empty ())

      fun doCounts (D.N (s, _, [])) = ns s
        | doCounts (D.N (s, _, [d])) = (ns s; doCounts d)
        | doCounts (D.N (s, _, deps)) =
            (case H.sub (counts, s) of
              SOME _ => ()
            | NONE =>
                let
                  val i = ref 0
                in
                  H.update (counts, s, (M.mutex (), i));
                  ns s;
                  app (fn d => (i := !i + 1; doCounts d)) deps
                end)

      fun cont (Cont (_, p, f)) =
            (case H.sub (nss, p) of
              SOME ns => cont (f ns)
            | NONE => raise Compile (Dependency p))
        | cont _ = ()

      fun postDep (n as D.N (s, _, _)) =
        case H.sub (counts, s) of
          NONE => FTP.submit (tp, comp n)
        | SOME (m, r) =>
            ( M.lock m
            ; r := !r - 1
            ; if !r = 0 before M.unlock m then
                FTP.submit (tp, comp n)
              else
                ()
            )

      and comp (D.N (s, bas, revs)) () =
        ( logElab log s
        ; cont (compileBas log copts ((SOME o valOf o H.sub) (nss, s)) s bas)
        ; app postDep revs
        )
    in
      doCounts root;
      app (fn n => comp n ()) leaves;
      case FTP.wait tp of
        NONE => (valOf o H.sub) (nss, s)
      | SOME e => raise e
    end

  fun parConc jobs (log, copts) ({ root as D.N (s, _, _), leaves, order, ... } : D.t) =
    let
      type c = int * (int * string) cont
      val sz = order * 5 div 4
      val nss : (NS.t * bool ref * c list ref * M.mutex) H.hash = H.hash sz
      val prios : (int * NS.t) H.hash = H.hash sz
      val tp = PTP.new jobs

      fun doPrio i (D.N (s, _, deps)) =
        case H.sub (prios, s) of
          SOME (j, ns) => if j < i then H.update (prios, s, (i, ns)) else ()
        | NONE =>
            let
              val ns = NS.empty ()
            in
              H.update (prios, s, (i, ns));
              H.update (nss, s, (ns, ref false, ref [], M.mutex ()));
              app (doPrio (i + 1)) deps
            end

      fun cont (Done ((_, p), _)) =
            let
              val (ns, b, l, m) = (valOf o H.sub) (nss, p)
            in
              M.lock m;
              b := true;
              M.unlock m;
              L.app
                (fn (p, f) => PTP.submit (tp, (p, fn () => cont (f ns))))
                (!l)
            end
        | cont (Cont ((prio, _), p, f)) =
            let
              val (ns, b, l, m) = (valOf o H.sub) (nss, p)
            in
              if !b then
                cont (f ns)
              else
                if (M.lock m; !b) then
                  (M.unlock m; cont (f ns))
                else
                  (l := (prio, f) :: !l; M.unlock m)
            end

      fun comp (D.N (s, bas, revs)) =
        case H.sub (prios, s) of
          NONE => ()
        | SOME (p, ns) =>
            ( H.delete (prios, s)
            ; PTP.submit
                (tp, (p, fn () =>
                  ( logElab log s
                  ; cont (compileBas log copts (SOME ns) (p, s) bas)
                  )))
            ; app comp revs
            )
    in
      doPrio 0 root;
      app comp leaves;
      case PTP.wait tp of
        NONE => (#1 o valOf o H.sub) (nss, s)
      | SOME e => raise e
    end

  fun numJobs j =
    if j <= 0 then
      Thread.Thread.numProcessors ()
    else
      Int.min (j, Thread.Thread.numProcessors ())

  fun compile log { copts, depsFirst, jobs } =
    (case (numJobs jobs, depsFirst) of
      (1, true) => serialDeps
    | (1, _)    => serialEncounter
    | (n, true) => parDeps n
    | (n, _)    => parConc n) (log, copts)
end
