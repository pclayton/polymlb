OS.FileSys.chDir "..";
use "src/lib/ThreadPool.sml";
OS.FileSys.chDir "test";

fun for (n, f) =
  let
    fun for' i = if i = n then () else (f i; for' (i + 1))
  in
    for' 0
  end

fun from (n, f) =
  let
    fun from' i = if i = 0 then () else (f i; from' (i - 1))
  in
    from' (n - 1)
  end

val MAX = 1000

structure Fifo = FifoFn (type elt = int);
structure Prio = PrioFn (type elt = int);

let
  val q = Fifo.new ()
in
  for (MAX, fn i => Fifo.enq (q, i));

  "Fifo queue keeps all elements in the correct order"
  assert
    List.tabulate (MAX, fn _ => valOf (Fifo.deq q))
  eq
    List.tabulate (MAX, fn i => i);

  "Fifo queue does not retain extra elements"
  assert Fifo.deq q eq NONE
end;

let
  val `^ = Word64.xorb
  val >> = Word64.>>

  infix 8 `^ >>

  val rand = (ref o Word64.fromLargeInt o Time.toMicroseconds o Time.now) ()

  fun sm64 () =
    let
      val _ = rand := !rand + 0wx9e3779b97f4a7c15
      val w = !rand
      val w = (w `^ (w >> 0w30)) * 0wxbf58476d1ce4e5b9
      val w = (w `^ (w >> 0w27)) * 0wx94d049bb133111eb
    in
      Word64.toInt ((w `^ (w >> 0w31)) >> 0w2) mod MAX
    end

  val elts = Array.tabulate (MAX, fn i => i)
  val q = Prio.new ()

  val sub = Array.sub
  val upd = Array.update
in
  from (MAX, fn i =>
    let
      val j = sm64 ()
      val z = sub (elts, i)
    in
      upd (elts, i, sub (elts, j));
      upd (elts, j, z)
    end);

  for (MAX, fn i =>
    let
      val j = sub (elts, i)
    in
      Prio.enq (q, (j, j))
    end);

  "Prio queue keeps all elements in the correct order"
  assert
    List.tabulate (MAX, fn i => valOf (Prio.deq q))
  eq
    List.tabulate (MAX, fn i => MAX - i - 1);

  "Prio queue does not retain extra elements"
  assert Prio.deq q eq NONE
end
