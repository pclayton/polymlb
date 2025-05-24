signature QUEUE =
sig
  type t
  type inc
  type outc
  val new : unit -> t
  val enq : t * inc -> unit
  val deq : t -> outc option
end
