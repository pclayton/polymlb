signature SIG =
sig
  val i : int
end

structure Str : SIG =
struct
  val i = 5
end
