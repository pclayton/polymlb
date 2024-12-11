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

  (* will match regardless of the value *)
  val exists : t -> t list -> bool

  val parse : string -> t option

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

  fun ann (_, "debug", SOME v) = O.map Debug (Bool.fromString v)
    | ann (_, "debug", _) = SOME (Debug false)
    | ann (_, "discard", SOME "") = SOME Discard
    | ann (_, "discard", _) = SOME Discard
    | ann (_, "ignoreFiles", SOME v) =
        ( O.map IgnoreFiles
        o O.filter (not o List.null)
        o S.tokens (fn c => c = #",")
        ) v
    | ann (_, "ignoreFiles", _) = SOME (IgnoreFiles [])
    | ann (true, "importAll", SOME "") = SOME ImportAll
    | ann (true, "importAll", _) = SOME ImportAll
    | ann _ = NONE

  fun parse s =
    let
      val (a, v) =
        SS.splitl (not o Char.isSpace) ((trimWS o SS.full) s)
    in
      case prefix a of
        ("", a) => ann (false, a, (SOME o SS.string o trimWS) v)
      | ("poly", a) => ann (true, a, (SOME o SS.string o trimWS) v)
      | _ => NONE
    end

  fun parseName s =
    case prefix (SS.full s) of
      ("", a) => ann (false, a, NONE)
    | ("poly", a) => ann (true, a, NONE)
    | _ => NONE
end
