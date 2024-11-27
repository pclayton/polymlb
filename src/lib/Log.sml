structure Log :>
sig
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * string
  type logger = { pathFmt : string -> string, print : event -> unit }

  val locFmt : (string -> string) -> PolyML.location -> string
end =
struct
  datatype level = Trace | Debug | Info | Warn | Error
  type event = level * string
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
end
