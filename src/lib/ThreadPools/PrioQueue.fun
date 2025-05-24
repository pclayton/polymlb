(* Skiplist priority queue from "Skiplist-based concurrent priority queues",
 * Itay Lotan and Nir Shavit.
 *
 * The usual level distribution of 1/2 is used, with a maximum level of 12;
 * this means that a single queue instance is suitable for upto 2^12 nodes
 * (4096). If needed, this arbitrary limit may be raised up to 31 simply by
 * modifying the MAX value; larger values would require changing the underlying
 * node type.
 * This implementation allows for elements with the same priority to be stored
 * within the same node upto a certain capacity (2^32) unless the existing node
 * is logically deleted at the time of insertion. If a new element cannot be
 * added to any existing node with the corresponding priority, a new node is
 * inserted _after_ all other nodes with the same priority. A node is considered
 * to be logically deleted when its element count reaches zero.
 * Thus dequeueing is simply traversing the lowest level (0) until a non empty
 * node is found, then its count is decreased by 1 and the element is retrieved.
 * If the updated count is zero, then the node is physically deleted from the
 * list.
 * Locking as well as capacity checks are implemented with a 63 bit bitset:
 * - the high 31 bits contain the capacity;
 * - the low MAX bits are used to lock forward pointers
 *   (level n is considered to be locked if 1 << n is set)
 * - the MAX + 1 bit is the insertion/deletion bit (full node lock).
 *
 * see:
 *   https://people.csail.mit.edu/shanir/publications/Priority_Queues.pdf
 *   https://dl.acm.org/doi/pdf/10.1145/78973.78977
 *)
functor PrioQueue (type elt) :> QUEUE
  where type inc = FixedInt.int * elt and type outc = elt =
struct
  structure A = Array
  structure C = Thread.ConditionVar
  structure M = Thread.Mutex

  type inc  = FixedInt.int * elt
  type outc = elt

  (* The tail node is a regular node that has a piority of Int.minInt; this
   * makes the overall code much cleaner since we can just check priorities
   * rather than casing each forward pointer
   * `case A.sub (a, i) of N _ => ... | _ => ()`
   *)
  datatype t =
    N of
      { p : FixedInt.int
      , v : elt list ref
      , a : t array
      , m : M.mutex
      , c : C.conditionVar
      , w : Word.word ref
      }

  val MAX = 12
  val INS = MAX + 1

  val `& = Word.andb
  val `| = Word.orb
  val `^ = Word.xorb
  val << = Word.<<
  val >> = Word.>>
  val `~ = Word.notb (* /!\ *)
  val  ~ = Word.~    (* /!\ *)

  infix 8 `& `| `^ << >>

  val LIM = (0w1 << 0w31) - 0w1

  (* Threadsafe biased RNG, 1/2 distribution from 1 to MAX.
   *
   * The generator used is xoroshiro128++, initially seeded with splitmix64;
   * generator state is thread local. Rather than the `while (rng () < 0.5) i++`
   * in Pugh's paper, we generate 63 bits once and keep only the low MAX bits,
   * discarding the rest.
   *
   * see:
   *   https://prng.di.unimi.it/
   *   https://prng.di.unimi.it/xoroshiro128plusplus.c
   *   https://xorshift.di.unimi.it/splitmix64.c
   *)
  local
    val t : (Word.word ref * Word.word ref) Universal.tag = Universal.tag ()
    structure T = Thread.Thread

    (* https://www.chessprogramming.org/Population_Count#The_PopCount_routine *)
    fun popcnt w =
      let
        val w = w - ((w >> 0w1) `& 0wx5555555555555555)
        val w = (w `& 0wx3333333333333333) + ((w >> 0w2) `& 0wx3333333333333333)
      in
        (((w + (w >> 0w4)) `& 0wxF0F0F0F0F0F0F0F) * 0wx101010101010101) >> 0w56
      end

    (* https://www.chessprogramming.org/BitScan#Index_of_LS1B_by_Popcount *)
    fun ctz 0w0 = 0w0
      | ctz w = popcnt ((w `& ~w) - 0w1)

    fun rotl (w, d) = (w << d) `| (w >> (0w64 - d))

    fun next (s0, s1) =
     let
        val w0 = !s0
        val w1 = !s1
        val r = rotl (w0 + w1, 0w17) + w0
        val w1 = w1 `^ w0
      in
        s0 := rotl (w0, 0w49) `^ w1 `^ (w1 << 0w21);
        s1 := rotl (w1, 0w28);
        r
      end

    (* Adjusted constants to avoid overflows *)
    fun sm64 w =
      let
        val _ = w := !w + 0wx4F1BBCDCBFA53E0B
        val w = !w
        val w = (w `^ (w >> 0w30)) * 0wx5FAC23B68E7272DD
        val w = (w `^ (w >> 0w27)) * 0wx4A6824DD899888F5
      in
        w `^ (w >> 0w31)
      end

    fun new () =
      let
        val s = (ref o Word.fromLargeInt o Time.toMicroseconds o Time.now) ()
      in
        (ref (sm64 s), ref (sm64 s))
      end

    val mask = (0w1 << Word.fromInt MAX) - 0w1
  in
    fun rand () =
      let
        val r =
          case T.getLocal t of
            SOME r => r
          | NONE => let val r = new () in T.setLocal (t, r); r end
      in
        (Word.toInt o ctz) (next r `& mask) + 1
      end
  end

  fun new () : t =
    let
      val tl =
        N { p = valOf FixedInt.minInt, v = ref [], a = A.fromList []
          , m = M.mutex (), c = C.conditionVar (), w = ref 0w0
          }
    in
      N { p = valOf FixedInt.maxInt, v = ref [], a = A.array (MAX, tl)
        , m = M.mutex (), c = C.conditionVar (), w = ref 0w0
        }
    end

  fun getp (N { p, ... }) = p
  fun getv (N { v, ... }) = v
  fun getn (N { a, ... }, i) = A.sub (a, i)

  local
    val min = valOf FixedInt.minInt
    val max = valOf FixedInt.maxInt
  in
    fun istl (N { p, ... }) = p = min
    fun ishd (N { p, ... }) = p = max
  end

  local
    val wpos = 0w32
    val wcnt = ((0w1 << 0w64) - 0w1) << wpos
    val wlm = (0w1 << wpos) - 0w1
  in
    fun getb w = w `& wlm
    fun getc w = (w `& wcnt) >> wpos
    fun incr w = ((getc w + 0w1) << wpos) + getb w
    fun decr w = ((getc w - 0w1) << wpos) + getb w

    fun setDel (N { w, m, ... }) =
      if !w `& wcnt = 0w0 then
        false
      else if (M.lock m; !w `& wcnt = 0w0) then
        (M.unlock m; false)
      else
        (w := decr (!w); M.unlock m; true)

    fun lock (N { w, m, c, ... }, i) =
      let
        val w' = 0w1 << Word.fromInt i
      in
        M.lock m;
        while !w `& w' <> 0w0 do
          C.wait (c, m);
        w := !w `| w';
        M.unlock m
      end

    fun unlock (N { w, m, c,... }, i) =
      let
        val w' = 0w1 << Word.fromInt i
      in
        M.lock m;
        w := !w `& `~w';
        M.unlock m;
        C.broadcast c
      end
  end

  fun getLock { n, p, i } =
    let
      fun f1 (n1, n2) = if getp n2 < p then n1 else f1 (n2, getn (n1, i))

      fun f2 (n1, n2) =
        if getp n2 < p then
          n1
        else
          (unlock (n1, i); lock (n2, i); f2 (n2, getn (n1, i)))

      val n1 = f1 (n, getn (n, i))
    in
      lock (n1, i);
      f2 (n1, getn (n1, i))
    end

  local
    fun fd (n1, n2, p, i) =
      if getp n2 < p then
        (n1, n2)
      else
        fd (n2, getn (n1, i), p, i)
  in
    fun preds (n, xp) =
      let
        val b = A.array (MAX, n)
        fun f (~1, n1, _) = n1
          | f (i, n1, n2) =
              let
                val (n1, n2) = fd (n1, n2, xp, i)
              in
                A.update (b, i, n1);
                f (i - 1, n1, n2)
              end
      in
        (b, f (MAX - 1, n, n))
      end
  end

  fun link { b, n' as N { a = a', p, ... }, n1 } =
    let
      val l = A.length a'
      fun f i =
        if i = l then
          ()
        else
          let
            val n as N { a, ... } =
              case (i, n1) of
                (0, SOME z) => z
              | _ => getLock { n = A.sub (b, i), p = p, i = i }
          in
            lock (n', i);
            A.update (a', i, A.sub (a, i));
            A.update (a, i, n');
            unlock (n', i);
            unlock (n, i);
            f (i + 1)
          end
    in
      lock (n', INS);
      f 0;
      unlock (n', INS)
    end

  fun enq (hd, (xp, xv)) =
    let
      val (b, n) = preds (hd, xp)
      val n1 as N { p, v, w, m, ... } = getLock { n = n, i = 0, p = xp }
    in
      if p = xp then
        let
          val w' = (M.lock m; getc (!w))
        in
          if w' = 0w0 orelse w' = LIM then
            let
              val _ = M.unlock m
              val n' =
                N { p = xp, v = ref [xv], a = A.array (rand (), hd)
                  , m = M.mutex (), c = C.conditionVar (), w = ref (incr 0w0)
                  }
            in
              link { b = b, n' = n', n1 = SOME n1 }
            end
          else
            ( v := xv :: !v
            ; w := incr (!w)
            ; M.unlock m
            ; unlock (n1, 0)
            )
        end
      else
        let
          val n' =
            N { p = xp, v = ref [xv], a = A.array (rand (), hd), m = M.mutex ()
              , c = C.conditionVar (), w = ref (incr 0w0)
              }
        in
          link { b = b, n' = n', n1 = SOME n1 }
        end
    end

  fun getLock { n, p, i, v } =
    let
      fun f1 (n1, n2) =
        if getp n2 > p orelse (getp n2 = p andalso getv n2 <> v) then
          f1 (n2, getn (n1, i))
        else
          n1

      fun f2 (n1, n2) =
        if getp n2 > p orelse (getp n2 = p andalso getv n2 <> v) then
          (unlock (n1, i); lock (n2, i); f2 (n2, getn (n1, i)))
        else
          n1

      val n1 = f1 (n, getn (n, i))
    in
      lock (n1, i);
      f2 (n1, getn (n1, i))
    end

  local
    fun fd (n1, n2, p, v, i) =
      if getp n2 > p orelse (getp n2 = p andalso getv n2 <> v) then
        fd (n2, getn (n1, i), p, v, i)
      else
        (n1, n2)
  in
    fun preds (n, xp, xv) =
      let
        val b = A.array (MAX, n)
        fun f (~1, n1, _) = n1
          | f (i, n1, n2) =
              let
                val (n1, n2) = fd (n1, n2, xp, xv, i)
              in
                A.update (b, i, n1);
                f (i - 1, n1, n2)
              end
      in
        (b, f (MAX - 1, n, n))
      end
  end

  fun find n =
    if istl n then
      n
    else if setDel n then
      n
    else
      find (getn (n, 0))

  fun del (b, n as N { a, p, v, ... }) =
    let
      fun f ~1 = ()
        | f i =
            let
              val n' as N { a = a', ... } =
                getLock { n = A.sub (b, i), i = i, p = p, v = v }
            in
              lock (n, i);
              A.update (a', i, A.sub (a, i));
              A.update (a, i, n');
              unlock (n, i);
              unlock (n', i);
              f (i - 1)
            end
    in
      lock (n, INS);
      f (A.length a - 1);
      unlock (n, INS)
    end

  fun deq hd =
    let
      val n as N { p, v, m, ... } = (find o getn) (hd, 0)
    in
      if istl n then
        NONE
      else
        case (M.lock m; !v) of
          [] => (M.unlock m; raise Fail "Prio.deq: impossible")
        | x::(xs as _::_) => (v := xs; M.unlock m; SOME x)
        | [v'] =>
            let
              val _ = v := []
              val _ = M.unlock m
              val (b, n) = preds (hd, p, v)

              fun f (n as N { p = p', v = v', ... }) =
                if p' = p  andalso v = v' then
                  n
                else
                  (f o getn) (n, 0)
            in
              del (b, f n);
              SOME v'
            end
    end
end
