structure Log :>
sig
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * (unit -> string)
  type pathFmt = string -> string
  type logger = { pathFmt : pathFmt, print : event -> unit }

  val log : logger option -> level -> (pathFmt -> string) -> unit

  val locFmt : pathFmt -> PolyML.location -> string

  val levelToInt : level -> int
  val levelFromInt : int -> level
end =
struct
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * (unit -> string)
  type pathFmt = string -> string
  type logger = { pathFmt : pathFmt, print : event -> unit }

  fun log NONE _ _ = ()
    | log (SOME { pathFmt, print }) l f = print (l, fn () => f pathFmt)

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
