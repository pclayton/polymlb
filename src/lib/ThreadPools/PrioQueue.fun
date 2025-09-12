(* Skiplist priority queue from "Skiplist-based concurrent priority queues",
 * Itay Lotan and Nir Shavit.
 *
 * The usual level distribution of 1/2 is used, with a maximum level of 8.
 * If needed, this arbitrary limit may be raised simply by modifying the
 * maxLevel value.
 * This implementation allows for elements with the same priority to be stored
 * within the same node upto a certain capacity unless the existing node is
 * logically deleted at the time of insertion. If a new element cannot be
 * added to any existing node with the corresponding priority, a new node is
 * inserted _after_ all other nodes with the same priority. A node is considered
 * to be logically deleted when its element count reaches zero.
 * Thus dequeueing is simply traversing the lowest level (0) until a non empty
 * node is found, then its count is decreased by 1 and the element is retrieved.
 * If the updated count is zero, then the node is physically deleted from the
 * list.
 * Locking as well as capacity checks are implemented with a bitset of size
 * usedBits:
 * - the low maxLevel bits are used to lock forward pointers
 *   (level n is considered to be locked if 1 << n is set);
 * - the next bit (maxLevel + 1) is the fullLock bit for insertion and deletion;
 * - the remaining high bits contain the capacity.
 * The capacity is always stored in the high bits, regardless of the word size.
 * E.g with usedBits = 31, maxLevel = 8 and a word size of 63, there is a 32 bit
 * gap in the middle of the bitset:
 * [22 bit capacity | 32 bit empty | 1 bit fullLock | 8 bit level locks] = 63.
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

  infix 8 `& `| `^ << >>

  val usedBits = 31
  val maxLevel = 8
  val fullLock = maxLevel + 1
  val maxItems = Word.<< (0w1, Word.fromInt (usedBits - fullLock)) - 0w1

  (* Threadsafe biased RNG, 1/2 distribution from 1 to maxLevel.
   *
   * The generator used is pcg16; generator state is thread local. Rather than
   * the `while (rng () < 0.5) i++` in Pugh's paper, we generate 16 bits once
   * and keep only the low maxLevel bits, discarding the rest.
   *
   * see:
   *   http://www.pcg-random.org
   *   https://github.com/imneme/pcg-c
   *)
  local
    structure T = Thread.Thread

    val op<< = Word16.<<
    val op>> = Word16.>>
    val op`& = Word16.andb
    val op`^ = Word16.xorb
    val    ~ = Word16.~

    val mask = (0w1 << Word.fromInt maxLevel) - 0w1
    val t : Word16.word ref Universal.tag = Universal.tag ()

    local
      (* https://www.chessprogramming.org/Population_Count#The_PopCount_routine *)
      fun popcnt w =
        let
          val w = w - ((w >> 0w1) `& 0wx5555)
          val w = (w `& 0wx3333) + ((w >> 0w2) `& 0wx3333)
        in
          (((w + (w >> 0w4)) `& 0wxF0F) * 0wx101) >> 0w8
        end

      (* https://www.chessprogramming.org/BitScan#Index_of_LS1B_by_Popcount *)
      fun ctz 0w0 = 0w0
        | ctz w = popcnt ((w `& ~w) - 0w1)

      (* Since the output size is 8 bits, i.e 256 possible values, we simply use
       * a lookup table for ctz.
       *)
      val counts = Word8Vector.tabulate
        ( Word16.toInt mask + 1
        , Word8.fromLarge o Word16.toLarge o ctz o Word16.fromInt
        )
    in
      fun biased w = Word8Vector.sub (counts, Word16.toInt w)
    end

    val tow = Word.fromLarge o Word16.toLarge

    (* pcg_oneseq_16_rxs_m_xs_16_random_r *)
    fun next (r as ref w) =
      let
        (* pcg_oneseq_16_step_r *)
        val _ = r := w * 0w12829 + 0w47989
        (* pcg_output_rxs_m_xs_16_16 *)
        val w = ((w >> tow ((w >> 0w13) + 0w3)) `^ w) * 0w62169
      in
        (w >> 0w11) `^ w
      end

    fun new () =
      (ref o Word16.fromInt o Int.fromLarge o Time.toMicroseconds o Time.now) ()
  in
    fun rand () =
      let
        val r =
          case T.getLocal t of
            SOME r => r
          | NONE => let val r = new () in T.setLocal (t, r); r end
      in
        (Word8.toInt o biased) (next r `& mask) + 1
      end
  end

  val op`& = Word.andb
  val op`| = Word.orb
  val op`^ = Word.xorb
  val op<< = Word.<<
  val op>> = Word.>>
  val   `~ = Word.notb (* /!\ *)
  val    ~ = Word.~    (* /!\ *)

  fun new () : t =
    let
      val tl =
        N { p = valOf FixedInt.minInt, v = ref [], a = A.fromList []
          , m = M.mutex (), c = C.conditionVar (), w = ref 0w0
          }
    in
      N { p = valOf FixedInt.maxInt, v = ref [], a = A.array (maxLevel, tl)
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
    val wpos = Word.fromInt (Word.wordSize - (usedBits - fullLock))
    val wlm = (0w1 << wpos) - 0w1
  in
    fun getb w = w `& wlm
    fun getc w = w >> wpos
    (* overflow warning: callers must check for w > 0 or w < maxItems *)
    fun incr w = ((getc w + 0w1) << wpos) + getb w
    fun decr w = ((getc w - 0w1) << wpos) + getb w

    fun setDel (N { w, m, ... }) =
      if !w >> wpos = 0w0 then
        false
      else if (M.lock m; !w >> wpos = 0w0) then
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
        val b = A.array (maxLevel, n)
        fun f (~1, n1, _) = n1
          | f (i, n1, n2) =
              let
                val (n1, n2) = fd (n1, n2, xp, i)
              in
                A.update (b, i, n1);
                f (i - 1, n1, n2)
              end
      in
        (b, f (maxLevel - 1, n, n))
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
      lock (n', fullLock);
      f 0;
      unlock (n', fullLock)
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
          if w' = 0w0 orelse w' = maxItems then
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
        val b = A.array (maxLevel, n)
        fun f (~1, n1, _) = n1
          | f (i, n1, n2) =
              let
                val (n1, n2) = fd (n1, n2, xp, xv, i)
              in
                A.update (b, i, n1);
                f (i - 1, n1, n2)
              end
      in
        (b, f (maxLevel - 1, n, n))
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
      lock (n, fullLock);
      f (A.length a - 1);
      unlock (n, fullLock)
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
