(* Two-lock fifo queue from "Simple, fast, and practical non-blocking and
 * blocking concurrent queue algorithms", Maged M. Michael and Michael L. Scott.
 * see:
 *   https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf
 *)
functor FifoQueue (type elt) :> QUEUE
  where type inc = elt and type outc = elt =
struct
  structure M = Thread.Mutex

  type inc  = elt
  type outc = elt

  datatype n = E | N of elt * n ref

  type t =
    { hd : n ref ref
    , tl : n ref ref
    , hm : M.mutex
    , tm : M.mutex
    }

  fun new () =
    let
      val r = ref E
    in
      { hd = ref r
      , tl = ref r
      , hm = M.mutex ()
      , tm = M.mutex ()
      }
    end

  fun enq ({ tl, tm, ... } : t, x) =
    let
      val r = ref E
    in
      M.lock tm;
      !tl := N (x, r);
      tl := r;
      M.unlock tm
    end

  fun deq ({ hd, hm, ... } : t) =
    case (M.lock hm; !(!hd)) of
      E => (M.unlock hm; NONE)
    | N (x, hd') => (hd := hd'; M.unlock hm; SOME x)
end
