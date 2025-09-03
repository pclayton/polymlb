structure Ann :
sig
  datatype t =
    (* Whether debug info should be included in the compiled code *)
    Debug of bool
    (* Completely ignore all enclosed declarations *)
  | Discard
    (* Ignore files with a matching base name *)
  | IgnoreFiles of string list
    (* Open the global namespace; only recognised if prefixed with `poly:` *)
  | ImportAll

  datatype res =
    Ann of t
  | BadArg of string
  | MissingArg
  | UnexpectedArg
  | Unrecognized

  (* will match regardless of the value *)
  val exists : t -> t list -> bool

  val parse : string -> res

  val parseName : string -> t option
end =
struct
  structure O  = Option
  structure S  = String
  structure SS = Substring

  datatype t =
    Debug of bool
  | Discard
  | IgnoreFiles of string list
  | ImportAll

  datatype res =
    Ann of t
  | BadArg of string
  | MissingArg
  | UnexpectedArg
  | Unrecognized

  fun chk (Debug _) (Debug _) = true
    | chk Discard Discard = true
    | chk (IgnoreFiles _) (IgnoreFiles _) = true
    | chk ImportAll ImportAll = true
    | chk _ _ = false

  fun exists a = List.exists (chk a)

  val trimWS = SS.dropr Char.isSpace o SS.dropl Char.isSpace

  fun prefix s =
    let
      val (p, a) = SS.splitl (fn c => c <> #":") s
    in
      if SS.size p = SS.size s then
        ("", SS.string s)
      else
        (SS.string p, (SS.string o SS.triml 1) a)
    end

  fun arg v =
    let
      val v = SS.string (trimWS v)
    in
      if size v = 0 then NONE else SOME v
    end

  fun parse s =
    let
      val (a, v) =
        SS.splitl (not o Char.isSpace) ((trimWS o SS.full) s)
      val (p, a) = prefix a
      val v = arg v
    in
      if size p > 0 andalso p <> "poly" then
        Unrecognized
      else if (p, a, v) = ("poly", "importAll", NONE) then
        Ann ImportAll
      else
        case (a, v) of
          ("debug", NONE) => Ann (Debug true)
        | ("debug", SOME v) =>
            (case Bool.fromString v of SOME b => Ann (Debug b) | _ => BadArg v)
        | ("discard", NONE) => Ann Discard
        | ("discard", _) => UnexpectedArg
        | ("ignoreFiles", v) =>
            (case O.map (S.tokens (fn c => c = #",")) v of
              SOME [] => MissingArg
            | SOME l => Ann (IgnoreFiles l)
            | _ => MissingArg)
        | _ => Unrecognized
    end

  fun parseName s =
    case s of
      "debug" => SOME (Debug true)
    | "discard" => SOME Discard
    | "ignoreFiles" => SOME (IgnoreFiles [])
    | _ => NONE
end
