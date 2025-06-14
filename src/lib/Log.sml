structure Log :>
sig
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * (unit -> string)
  type logger = { pathFmt : string -> string, print : event -> unit }

  val locFmt : (string -> string) -> PolyML.location -> string

  val levelToInt : level -> int
  val levelFromInt : int -> level
end =
struct
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * (unit -> string)
  type logger = { pathFmt : string -> string, print : event -> unit }

  val int = Int.toString

  fun locFmt fmt { file, startLine, startPosition, endLine, endPosition } =
    String.concat
      ( fmt file ^ ":"
      :: (if startLine = 0 then
          []
        else
          [ int startLine, ".", int startPosition, "-"
          , int endLine, ".", int endPosition
          ])
      )

  fun levelToInt l =
    case l of
      Error => 1
    | Warn  => 2
    | Info  => 3
    | Debug => 4
    | Trace => 5

  fun levelFromInt i =
    case i of
      1 => Error
    | 2 => Warn
    | 3 => Info
    | 4 => Debug
    | 5 => Trace
    | _ => raise Fail ("Log.levelFromInt: invalid input: " ^ Int.toString i)
end
