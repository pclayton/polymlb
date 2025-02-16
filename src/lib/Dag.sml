structure Dag :
sig
  datatype node = N of int * node vector

  type t =
    { root   : node
    , leaves : node vector
    , bases  : Basis.t vector
      (* raises on invalid path *)
    , id     : string -> int
      (* raises on invalid id *)
    , path   : int -> string
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
  structure A  = Array
  structure AS = ArraySlice
  structure H  = HashArray
  structure L  = List
  structure V  = Vector

  structure Buffer :>
  sig
    type 'a t

    val new : int * 'a -> 'a t
    val cnt : 'a t -> int

    val add : 'a t * 'a -> unit
    val sub : 'a t * int -> 'a
    val set : 'a t * int * 'a -> unit
    val clear : 'a t -> unit
    val addIfAbsent : ('a * 'a -> bool) -> 'a t * 'a -> unit

    val slice : 'a t -> 'a AS.slice
    val array : 'a t -> 'a array
    val vec   : 'a t -> 'a vector
  end =
  struct
    type 'a t = int ref * 'a * 'a array ref

    fun new (i, x) = (ref 0, x, (ref o A.array) (i, x))

    fun cnt (ref i, _, _) = i

    fun resize ((ri, x, ra as ref a), n) =
      let
        val a' = A.array (Int.max (A.length a * 2, n), x)
      in
        AS.copy { src = AS.slice (a, 0, SOME (!ri)), dst = a', di = 0 };
        ra := a';
        ()
      end

    fun add (t as (ri, _, ra), x) =
      ( if !ri = A.length (!ra) then resize (t, 1) else ()
      ; A.update (!ra, !ri, x)
      ; ri := !ri + 1
      )

    fun sub ((_, _, ref a), i) = A.sub (a, i)

    fun set (t as (ri, _, ra), i, x) =
      ( if i >= A.length (!ra) then resize (t, i + 1) else ()
      ; A.update (!ra, i, x)
      ; ri := Int.max (!ri, i) + 1
      )

    fun addIfAbsent eq (t as (ref i, _, ref a), x) =
      let
        fun f j = i <> j andalso (eq (x, A.sub (a, j)) orelse f (j + 1))
      in
        if f 0 then
          ()
        else
          add (t, x)
      end

    fun slice (ref i, _, ref a) = AS.slice (a, 0, SOME i)

    fun array (ref i, _, ref a) = A.tabulate (i, fn j => A.sub (a, j))

    fun vec z = AS.vector (slice z)

    fun clear (t as (ri, x, _)) = (AS.modify (fn _ => x) (slice t); ri := 0)
  end

  structure B  = Buffer

  datatype node = N of int * node vector

  type t =
    { root   : node
    , leaves : node vector
    , bases  : Basis.t vector
    , id     : string -> int
    , path   : int -> string
    }

  type ir =
    { bases : Basis.t vector
    , paths : string vector
    , ids   : int H.hash
    , deps  : int B.t B.t
    , revs  : int B.t B.t
    }

  datatype err = Cycle of string list

  exception Dag of err

  type opts =
    { logger : Log.logger option
    }

  fun hsub z = Option.valOf (H.sub z)

  local
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

  val baseSize = 10

  (* Depth first so that any cycle found is the first one when reading
   * sequentially from the root.
   *)
  fun traverse getBas root : ir =
    let
      val bases : Basis.t B.t = B.new (baseSize * 2, [])
      val paths : string B.t = B.new (baseSize * 2, "")
      val ids   : int H.hash = H.hash (baseSize * 2)
      val deps  = B.new (baseSize, B.new (0, ~1))
      val revs  = B.new (baseSize, B.new (0, ~1))

      fun dec ([], _, _, _) = ()
        | dec (Basis (_, e) :: ds, id, ps, is) =
            (exp (e, id, ps, is); dec (ds, id, ps, is))
        | dec (BasisFile p :: ds, id, ps, is) =
            if L.exists (fn p' => p = p') is then
              dec (ds, id, ps, is)
            else
              let
                val id' =
                  case H.sub (ids, p) of
                    SOME id =>
                      (case index (ps, p) of
                        ~1 => id
                      | i => raise (Dag o Cycle) (p :: (rev o L.take) (ps, i)))
                  | NONE =>
                      let
                        val ds' = getBas p
                        val id' = B.cnt bases
                      in
                        H.update (ids, p, id');
                        B.add (paths, p);
                        B.add (deps, B.new (id' + 1, ~1));
                        B.add (revs, B.new (id' + 1, ~1));
                        B.add (bases, ds');
                        dec (ds', id', p::ps, []);
                        id'
                      end
              in
                B.addIfAbsent op= (B.sub (deps, id), id');
                (* B.add (B.sub (deps, id), id'); *)
                B.addIfAbsent op= (B.sub (revs, id'), id);
                (* B.add (B.sub (revs, id'), id); *)
                dec (ds, id, ps, is)
              end
        | dec (Ann (l, ds') :: ds, id, ps, is) =
            ( if Ann.exists Ann.Discard l then
                ()
              else
                dec (ds', id, ps,
                  L.foldl
                    (fn (Ann.IgnoreFiles f, fs) => f @ fs | (_, fs) => fs)
                    is l)
            ; dec (ds, id, ps, is)
            )
        | dec (Local (ds1, ds2) :: ds, id, ps, is) =
            (dec (ds1, id, ps, is); dec (ds2, id, ps, is); dec (ds, id, ps, is))
        | dec (_::ds, id, ps, is) = dec (ds, id, ps, is)

      and exp (Bas ds, id, ps, is) = dec (ds, id, ps, is)
        | exp (Let (ds, e), id, ps, is) = (dec (ds, id, ps, is); exp (e, id, ps, is))
        | exp (Id _, _, _, _) = ()

      val bas = getBas root
    in
      H.update (ids, root, 0);
      B.set (bases, 0, bas);
      B.set (paths, 0, root);
      B.set (deps, 0, B.new (baseSize, ~1));
      B.set (revs, 0, B.new (baseSize, ~1));
      dec (bas, 0, [root], []);

      { bases = B.vec bases
      , paths = B.vec paths
      , ids   = ids
      , deps  = deps
      , revs  = revs
      }
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
    fun reduce (ir as { bases, deps, revs, ... } : ir) : ir =
      let
        val sz = V.length bases
        val m = M.new sz
        val s = S.new sz

        fun f id =
          if (not o S.sub) (s, id) then
            ( S.set (s, id)
            ; (B.clear o B.sub) (revs, id)
            ; ( AS.app (fn id' => (M.set (m, id, id'); f id'))
              o B.slice
              o B.sub
              ) (deps, id)
            )
          else
            ()

        fun update id =
          if (not o S.sub) (s, id) then
            let
              val b = B.sub (deps, id)
              val l = AS.foldr
                (fn (id', ids) => if M.sub (m, id, id') then id'::ids else ids)
                [] (B.slice b)
            in
              S.set (s, id);
              B.clear b;
              app (fn id' =>
                ( B.add (b, id')
                ; B.add (B.sub (revs, id'), id)
                ; update id'
                )) l
            end
          else
            ()
      in
        (* construct edge matrix *)
        f 0;
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
        update 0;

        ir
      end
  end

  local
    val dummy = N (~1, V.fromList [])
    fun isDummy (N (i, _)) = i = ~1
  in
    fun mkDag ({ bases, paths, ids, deps, revs } : ir) : t =
      let
        val ndeps  = A.array (V.length bases, dummy)
        val nrevs  = A.array (V.length bases, dummy)
        val leaves = B.new (baseSize, ~1)

        fun dep id =
          if (not o isDummy o A.sub) (ndeps, id) then
            A.sub (ndeps, id)
          else
            let
              val n as N (_, v) =
                N ( id
                  , let
                      val b = B.sub (deps, id)
                    in
                      V.tabulate (B.cnt b, fn i => (dep o B.sub) (b, i))
                    end
                  )
            in
              A.update (ndeps, id, n);
              if V.length v = 0 then B.add (leaves, id) else ();
              n
            end

        fun rev id =
          if (not o isDummy o A.sub) (nrevs, id) then
            A.sub (nrevs, id)
          else
            let
              val n =
                N ( id
                  , let
                      val b = B.sub (revs, id)
                    in
                      V.tabulate (B.cnt b, fn i => (rev o B.sub) (b, i))
                    end
                  )
            in
              A.update (nrevs, id, n);
              n
            end
      in
        { root   = dep 0
        , leaves = V.tabulate (B.cnt leaves, fn i => (rev o B.sub) (leaves, i))
        , bases  = bases
        , id     = fn s => (valOf o H.sub) (ids, s)
        , path   = fn i => V.sub (paths, i)
        }
      end
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
