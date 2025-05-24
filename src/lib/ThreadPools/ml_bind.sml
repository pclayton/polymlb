structure ThreadPools =
let
  structure Fifo = FifoQueue (type elt = unit -> unit)
  structure Prio = PrioQueue (type elt = unit -> unit)
in
  struct
    structure FTP = ThreadPool (struct open Fifo fun conv x = x end)
    structure PTP = ThreadPool (struct open Prio fun conv (_, x) = x end)
  end
end
