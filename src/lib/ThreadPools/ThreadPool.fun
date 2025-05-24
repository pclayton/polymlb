(* Simple bounded threadpool implementation with centralized task queue.
 * If, when submitting a new task, the thread count is less than the max, then
 * fork immediately; otherwise, add to the queue. Upon task completion, threads
 * pop the queue and go again, until the queue is empty.
 * If a task fails (i.e raises an exception), threads are sent an interrupt and
 * will not process further tasks.
 *)
functor ThreadPool (Q :
  sig
    include QUEUE where type outc = unit -> unit
    val conv : inc -> outc
  end) :>
sig
  type t
  val new    : int -> t
  val submit : t * Q.inc -> unit
  (* Blocks until all threads have terminated; either because the task queue
   * is empty or a task has failed, in which case the corresponding exn is
   * returned.
   *)
  val wait   : t -> exn option
end =
struct
  structure A = Array
  structure C = Thread.ConditionVar
  structure M = Thread.Mutex
  structure T = Thread.Thread

  fun index a =
    let
      val i = ref 0
    in
      while !i < A.length a andalso (Option.isSome o A.sub) (a, !i) do
        i := !i + 1;
      !i
    end

  type t =
    { max : int
    , cur : int ref
    , threads : T.thread option array
    , q : Q.t
    , m : M.mutex
    , exit : C.conditionVar
    , err : exn option ref
    }

  fun new i =
    { max = i
    , cur = ref 0
    , threads = A.tabulate (i, fn _ => NONE)
    , q = Q.new ()
    , m = M.mutex ()
    , exit = C.conditionVar ()
    , err = ref NONE
    }

  fun run (i, f, { cur, threads, m, q, exit, err, ... } : t) =
    let
      val ok = ref true
      fun call f =
        (T.testInterrupt (); f ())
          handle
            T.Interrupt => ok := false
          | e =>
              ( ok := false
              ; M.lock m
              ; if (not o Option.isSome o !) err then
                  ( err := SOME e
                  ; M.unlock m
                  ; A.app (fn SOME t => T.interrupt t | _ => ()) threads
                  )
                else
                  M.unlock m
              )
    in
      call f;
      while !ok do
        case Q.deq q of
          SOME f => call f
        | NONE => ok := false;
      M.lock m;
      A.update (threads, i, NONE);
      cur := !cur - 1;
      M.unlock m;
      C.signal exit
    end

  fun submit (t as { max, cur, threads, q, m, err, ... } : t, f) =
    if (M.lock m; Option.isSome (!err)) then
      M.unlock m
    else if !cur = max then
      (M.unlock m; Q.enq (q, f))
    else
      let
        val i = index threads
      in
        cur := !cur + 1;
        A.update
          (threads, i, (SOME o T.fork) (fn () => run (i, Q.conv f, t), []));
        M.unlock m
      end

  fun wait ({ cur, m, exit, err, ... } : t) =
    ( M.lock m
    ; while !cur > 0 do
        C.wait (exit, m)
    ; M.unlock m
    ; !err before err := NONE
    )
end
