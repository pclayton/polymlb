structure Dag :
sig
  datatype node = N of string * Basis.t * node list

  type t =
    { root   : node
    , leaves : node list
    , bas    : string -> Basis.t
    , order  : int
    }

  datatype err = Cycle of string list

  exception Dag of err

  type opts =
    { logger : Log.logger option
    }

  (* Traverse and reduce to a minimal equivalent the DAG formed by having the
   * given basis as root and all other bases which are transitively reachable
   * through basis file imports.
   * Declarations annotated with Discard are completely ignored,
   * as well as MLB files which match an enclosing IgnoreFiles annotation.
   * If a cycle is found, then a Dag exception is raised.
   * The given function takes in the absolute path of an mlb file and must
   * return its content.
   *)
  val process : opts -> (string -> Basis.t) -> string -> t
end =
struct
  structure H = HashArray
  structure L = List

  datatype node = N of string * Basis.t * node list

  type t =
    { root   : node
    , leaves : node list
    , bas    : string -> Basis.t
    , order  : int
    }

  type ir =
    { root  : string
    , bases : (int * Basis.t) H.hash
    , deps  : int H.hash H.hash
    , revs  : int H.hash H.hash
    , order : int
    }

  datatype err = Cycle of string list

  exception Dag of err

  type opts =
    { logger : Log.logger option
    }

  fun hsub z = Option.valOf (H.sub z)

  local
    structure A = Array
    structure W = Word

    val `& = W.andb
    val `| = W.orb
    val << = W.<<
    val ~  = W.notb

    infix 8 `& `| <<

    val itor = Real.fromInt
    val itow = W.fromInt

    val ws = W.wordSize

    local
      (* We assume that Word.wordSize is 63.
       * Should it be even, this should be `ws - 1`.
       * Additionally, it'd likely be a power of 2, in which case all the div
       * ops may be replaced with right shifts by a value BITS such that
       * `0w1 << BITS = wordSize`, e.g 0w6 for Word64.
       *)
      val w' = itow ws
    in
      fun bit w = 0w1 << (w `& w')
    end
  in
    structure BitSet :>
    sig
      type t
      val new : int -> t
      val sub : t * int -> bool
      val set : t * int -> unit
      val del : t * int -> unit
      val clear : t -> unit
    end =
    struct
      type t = word A.array

      fun new i = A.array (Real.ceil (itor i / itor ws), 0w0)

      fun sub (a, i) = A.sub (a, i div ws) `& bit (itow i) <> 0w0

      fun set (a, i) =
        let
          val j = i div ws
        in
          A.update (a, j, A.sub (a, j) `| bit (itow i))
        end

      fun del (a, i) =
        let
          val j = i div ws
        in
          A.update (a, j, A.sub (a, j) `& (~ o bit o itow) i)
        end

      val clear = A.modify (fn _ => 0w0)
    end

    structure BitMatrix :>
    sig
      type t
      val new : int -> t
      val sub : t * int * int -> bool
      val set : t * int * int -> unit
      val del : t * int * int -> unit
      val clear : t -> unit
    end =
    struct
      type t = word A.array * int

      fun new i = (A.array (Real.ceil (itor (i * i) / itor ws), 0w0), i)

      fun idx (c, i, j) =
        let
          val n = i * c + j
        in
          (n div ws, n mod ws)
        end

      fun sub ((a, c), i, j) =
        let
          val (i, j) = idx (c, i, j)
        in
          A.sub (a, i) `& bit (itow j) <> 0w0
        end

      fun set ((a, c), i, j) =
        let
          val (i, j) = idx (c, i, j)
        in
          A.update (a, i, A.sub (a, i) `| bit (itow j))
        end

      fun del ((a, c), i, j) =
        let
          val (i, j) = idx (c, i, j)
        in
          A.update (a, i, A.sub (a, i) `& (~ o bit o itow) j)
        end

      fun clear (a, _) = A.modify (fn _ => 0w0) a
    end
  end

  structure S = BitSet
  structure M = BitMatrix

  fun index (l, s) =
    let
      fun idx ([], _) = ~1
        | idx (x::xs, i) = if x = s then i else idx (xs, i + 1)
    in
      idx (l, 0)
    end

  fun upd (h, k, v, id) =
    case H.sub (h, k) of
      SOME m => H.update (m, v, id)
    | NONE =>
        let
          val m : int H.hash = H.hash 10
        in
          H.update (m, v, id);
          H.update (h, k, m)
        end

  datatype z = datatype Basis.dec
  datatype z = datatype Basis.exp

  (* Depth first so that any cycle found is the first one when reading
   * sequentially from the root.
   * Nested hash maps instead of list H.hash to filter out duplicates.
   *)
  fun traverse getBas root : ir =
    let
      val bases : (int * Basis.t) H.hash = H.hash 10
      val deps  : int H.hash H.hash = H.hash 10
      val revs  : int H.hash H.hash = H.hash 10
      val order = ref 1

      fun dec ([], _, _) = ()
        | dec (Basis (_, e) :: ds, ps, is) =
            (exp (e, ps, is); dec (ds, ps, is))
        | dec (BasisFile p :: ds, ps, is) =
            if L.exists (fn p' => p = p') is then
              dec (ds, ps, is)
            else
              ( case H.sub (bases, p) of
                  SOME _ =>
                    (case index (ps, p) of
                      ~1 => ()
                    | i => raise (Dag o Cycle) (p :: (rev o L.take) (ps, i)))
                | NONE =>
                    let
                      val ds' = getBas p
                    in
                      H.update (bases, p, (!order, ds'));
                      order := !order + 1;
                      dec (ds', p::ps, [])
                    end
              ; upd (deps, hd ps, p, (#1 o hsub) (bases, p))
              ; upd (revs, p, hd ps, (#1 o hsub) (bases, hd ps))
              ; dec (ds, ps, is)
              )
        | dec (Ann (l, ds') :: ds, ps, is) =
            ( if Ann.exists Ann.Discard l then
                ()
              else
                dec (ds', ps,
                  L.foldl
                    (fn (Ann.IgnoreFiles f, fs) => f @ fs | (_, fs) => fs)
                    is l)
            ; dec (ds, ps, is)
            )
        | dec (Local (ds1, ds2) :: ds, ps, is) =
            (dec (ds1, ps, is); dec (ds2, ps, is); dec (ds, ps, is))
        | dec (_::ds, ps, is) = dec (ds, ps, is)

      and exp (Bas ds, ps, is) = dec (ds, ps, is)
        | exp (Let (ds, e), ps, is) = (dec (ds, ps, is); exp (e, ps, is))
        | exp (Id _, _, _) = ()

      val bas = getBas root
    in
      H.update (bases, root, (0, bas));
      dec (bas, [root], []);
      { root = root, bases = bases, deps = deps, revs = revs, order = !order }
    end

  (* Hsu's algorithm for transitive reduction; "An algorithm for finding a
   * minimal equivalent graph of a digraph", ACM, 22(1):11-16.
   * See:
   *   https://projects.csail.mit.edu/jacm/References/hsu1975:11.html
   *   https://dl.acm.org/doi/10.1145/321864.321866
   *   https://stackoverflow.com/a/16357676
   *)
  local
    infix ++>
    fun n ++> f =
      let
        val i = ref 0
      in
        while !i <> n do
          (f (!i); i := !i + 1)
      end
  in
    fun reduce ({ root, bases, deps, revs, order } : ir) : ir =
      let
        val sz = order
        val m = M.new sz
        val s = S.new sz

        fun f (p, id) =
          if (not o S.sub) (s, id) then
            case (S.set (s, id); H.sub (deps, p)) of
              NONE => ()
            | SOME ds =>
                H.fold
                  (fn (p', id', ()) => (M.set (m, id, id'); f (p', id')))
                  () ds
          else
            ()

        fun update (p, id) =
          if (not o S.sub) (s, id) then
            case (S.set (s, id); H.sub (deps, p)) of
              NONE => ()
            | SOME ds =>
                H.fold
                  (fn (p', id', ()) =>
                    ( if (not o M.sub) (m, id, id') then
                        (H.delete (ds, p'); H.delete (hsub (revs, p'), p))
                      else
                        ()
                    ; update (p', id')
                    ))
                  () ds
          else
            ()
      in
        (* construct edge matrix *)
        f (root, 0);
        S.clear s;

        (* transform edge- into path matrix *)
        sz ++> (fn i =>
          sz ++> (fn j =>
            if i = j orelse (not o M.sub) (m, j, i) then
              ()
            else
              sz ++> (fn k =>
                if (not o M.sub) (m, j, k) andalso M.sub (m, i, k) then
                  M.set (m, j, k)
                else
                  ())));

        (* unset unwanted edges *)
        sz ++> (fn j =>
          sz ++> (fn i =>
            if M.sub (m, i, j) then
              sz ++> (fn k =>
                if M.sub (m, j, k) then
                  M.del (m, i, k)
                else
                  ())
            else
              ()));

        (* delete from the graph *)
        update (root, 0);

        { root = root, bases = bases, deps = deps, revs = revs, order = order }
      end
  end

  fun mkDag ({ root, bases, deps, revs, order } : ir) : t =
    let
      val empty  : int  H.hash = H.hash 1
      val ndeps  : node H.hash = H.hash order
      val nrevs  : node H.hash = H.hash order
      val leaves : unit H.hash = H.hash 4

      fun dep s =
        case H.sub (ndeps, s) of
          SOME n => n
        | NONE =>
            let
              val n as N (_, _, l) =
                N ( s
                  , (#2 o hsub) (bases, s)
                  , ( map dep
                    o H.fold (fn (k, _, l) => k::l) []
                    o getOpt
                    ) (H.sub (deps, s), empty)
                  )
            in
              H.update (ndeps, s, n);
              if L.null l then H.update (leaves, s, ()) else ();
              n
            end

      fun rev s =
        case H.sub (nrevs, s) of
          SOME n => n
        | NONE =>
            let
              val n =
                N ( s
                  , (#2 o hsub) (bases, s)
                  , ( map rev
                    o H.fold (fn (k, _, l) => k::l) []
                    o getOpt
                    ) (H.sub (revs, s), empty)
                  )
            in
              H.update (nrevs, s, n);
              n
            end
    in
      { root   = dep root
      , leaves = map rev (H.fold (fn (k, _, l) => k::l) [] leaves)
      , bas    = fn s => (#2 o hsub) (bases, s)
      , order  = order
      }
    end

  fun process { logger } f s =
    let
      val (log, parse) =
        case logger of
          NONE => (fn _ => (), f)
        | SOME { pathFmt, print} =>
            ( fn m => print (Log.Debug, fn () => m)
            , fn s => (print (Log.Debug, fn () => "parsing " ^ pathFmt s); f s)
            )
      fun (m @ f) z = (log m; f z)
    in
      ( "building MLB graph"   @ mkDag
      o "reducing MLB graph"   @ reduce
      o "traversing MLB graph" @ traverse parse
      ) s
    end
end
