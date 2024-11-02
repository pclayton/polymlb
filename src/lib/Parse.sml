structure Parse :
sig
  (* Stringly typed version of Basis.dec and Basis.exp augmented with location.
   * withtype not allowed in signatures, see in struct for cleaner version.
   *)
  datatype dec_kind =
    Basis of (string * (exp_kind * PolyML.location)) list
  | File of string
  | Ann of string list * (dec_kind * PolyML.location) list
  | Local of (dec_kind * PolyML.location) list * (dec_kind * PolyML.location) list
  | Open of string list
  | Structure of (string * string) list
  | Signature of (string * string) list
  | Functor of (string * string) list

  and exp_kind =
    Bas of (dec_kind * PolyML.location) list
  | Id of string
  | Let of (dec_kind * PolyML.location) list * (exp_kind * PolyML.location)

  type dec = dec_kind * PolyML.location
  type exp = exp_kind * PolyML.location

  type t = dec list

  type opts = { fileName : string, lineOffset : int }

  datatype expected =
    Dec
  | Exp
  | ShortId
  | LongId
  | String

  datatype found =
    EOS
  | Invalid of string

  type err = { expected : expected, found : found, at : PolyML.location }

  exception Parse of err

  val parse : opts -> string -> t
end =
struct
  structure L = Lex
  structure T = L.Token

  datatype dec_kind =
    Basis of (string * exp) list
  | File of string
  | Ann of string list * dec list
  | Local of dec list * dec list
  | Open of string list
  | Structure of (string * string) list
  | Signature of (string * string) list
  | Functor of (string * string) list

  and exp_kind =
    Bas of dec list
  | Id of string
  | Let of dec list * exp

  withtype dec = dec_kind * PolyML.location
       and exp = exp_kind * PolyML.location

  type t = dec list

  type opts = { fileName : string, lineOffset : int }

  datatype expected =
    Dec
  | Exp
  | ShortId
  | LongId
  | String

  datatype found =
    EOS
  | Invalid of string

  type err = { expected : expected, found : found, at : PolyML.location }

  exception Parse of err

  fun mkLoc (f, l, c, l', c') =
    { file = f
    , startLine = l, endLine = l'
    , startPosition = c, endPosition = c'
    }

  type 'a parser = L.state -> 'a L.res

  infix 2 <& &> <&>
  infix 1 \ \:

  (* mandatory *)
  fun ! e p (src as (_, _, _, ss)) =
    case p src of
      NONE =>
        let
          val (s, loc) = L.any src
          val f = if Substring.size ss = 0 then EOS else Invalid s
        in
          raise Parse { expected = e, found = f, at = loc }
        end
    | z => z

  (* discard 2nd *)
  fun (p1 <& (p2 : 'a parser)) (src as (p, l, c, _)) =
    case p1 src of
      NONE => NONE
    | SOME (r, _, src) =>
        (case p2 src of
          NONE => NONE
        | SOME (_, { endLine, endPosition, ... }, src) =>
            SOME (r, mkLoc (p, l, c, endLine, endPosition), src))

  (* discard 1st *)
  fun (p1 &> (p2 : 'a parser)) (src as (p, l, c, _)) =
    case p1 src of
      NONE => NONE
    | SOME (_, _, src) =>
        (case p2 src of
          NONE => NONE
        | SOME (r, { endLine, endPosition, ... }, src) =>
            SOME (r, mkLoc (p, l, c, endLine, endPosition), src))

  (* combine *)
  fun (p1 <&> (p2 : 'a parser)) (src as (p, l, c, _)) =
    case p1 src of
      NONE => NONE
    | SOME (r1, _, src) =>
        (case p2 src of
          NONE => NONE
        | SOME (r2, { endLine, endPosition, ... }, src) =>
            SOME ((r1, r2), mkLoc (p, l, c, endLine, endPosition), src))

  (* map *)
  fun (p \ f) src =
    case p src of
      NONE => NONE
    | SOME (r, l, src) => SOME (f r, l, src)

  (* map with location *)
  fun (p \: f) src =
    case p src of
      NONE => NONE
    | SOME (r, l, src) => SOME (f (r, l), l, src)

  fun any ps src =
    let
      fun f [] = NONE
        | f (p::ps) =
            case p src of
              NONE => f ps
            | r => r
    in
      f ps
    end

  fun seq (ps : 'a parser list) (src as (s, l, c, _)) =
    let
      fun f ([], src, l', c', rs) =
            SOME (List.rev rs, mkLoc (s, l, c, l', c'), src)
        | f (p::ps, src, _, _, rs) =
            case p src of
              NONE => NONE
            | SOME (r, loc as { endLine, endPosition, ... }, src) =>
                f (ps, src, endLine, endPosition, (r, loc)::rs)
    in
      f (ps, src, l, c, [])
    end

  fun star (p : 'a parser) (src as (s, l, c, _)) =
    let
      fun f (src, l', c', rs) =
        case p src of
          NONE => SOME (List.rev rs, mkLoc (s, l, c, l', c'), src)
        | SOME (r,  { endLine, endPosition, ... }, src) =>
            f (src, endLine, endPosition, r::rs)
    in
      f (src, l, c, [])
    end

  fun plus (p : 'a parser) (src as (s, l, c, _)) =
    let
      fun f (src, l', c', rs) =
        case p src of
          NONE => SOME (List.rev rs, mkLoc (s, l, c, l', c'), src)
        | SOME (r, { endLine, endPosition, ... }, src) =>
            f (src, endLine, endPosition, r::rs)
    in
      case p src of
        NONE => NONE
      | SOME (r, { endLine, endPosition, ... }, src) =>
          f (src, endLine, endPosition, [r])
    end

  fun until p1 (p2 : 'a parser) (src as (s, l, c, _)) =
    let
      fun f (src, l', c', rs) =
        case p1 src of
          NONE =>
            (case p2 src of
              NONE => NONE
            | SOME (r, { endLine, endPosition, ... }, src) =>
                f (src, endLine, endPosition, r::rs))
        | SOME (_, _, src) =>
            SOME (List.rev rs, mkLoc (s, l, c, l', c'), src)
    in
      f (src, l, c, [])
    end

  fun maybe p (src as (s, l, c, _)) =
    case p src of
      NONE => SOME (NONE, mkLoc (s, l, c, l, c), src)
    | SOME (r, loc, src) => SOME (SOME r, loc, src)

  fun id k src =
    let
      val e = case k of T.Id => ShortId | T.LongId => LongId
    in
      ! e (L.id k) src
    end

  fun bind (base, idKind) =
    let
      fun f kw =
        (L.res kw &> id idKind) <&> maybe (L.res T.Eq &> id idKind)
        \ (fn (z, opt) => (z, getOpt (opt, z)))
    in
      f base <&> star (f T.And) \ op::
    end

  val inp = L.res T.In

  val endp = L.res T.End

  fun basBind src =
    let
      fun f kw = L.res kw &> id T.Id <& L.res T.Eq <&> exp
    in
      (f T.Basis <&> star (f T.And) \ op:: \ Basis) src
    end

  and strBind src = (bind (T.Structure, T.LongId) \ Structure) src

  and sigBind src = (bind (T.Signature, T.Id) \ Signature) src

  and funBind src = (bind (T.Functor, T.Id) \ Functor) src

  and localIn src =
    ( L.res T.Local &> decs inp <&> decs endp
    \ Local
    ) src

  and openBas src =
    ( L.res T.Open &> ! ShortId (plus (L.id T.Id))
    \ Open
    ) src

  and ann src =
    ( L.res T.Ann &> ! String (plus L.ann) <& inp <&> decs endp
    \ Ann
    ) src

  and file src = (L.path \ File) src

  and dec src  =
    (any [basBind, strBind, sigBind, funBind, localIn, openBas, ann, file]
    \: (fn (r, loc) => (r, loc))) src

  and decs p src = until p dec src

  and basExp src =
    (L.res T.Bas &> decs endp \ Bas) src

  and letIn src =
    (L.res T.Let &> decs inp <&> exp <& endp \ Let) src

  and basId src = (L.id T.Id \ Id) src

  and exp src =
    (! Exp (any [basExp, letIn, basId] \: (fn (r, loc) => (r, loc)))) src

  local
    fun find (f, l, v) =
      let
        fun fd [] = NONE
          | fd (x::xs) = case f x of NONE => fd xs | z => z
      in
        Option.getOpt (fd l, v)
      end

    fun check (z, src as (_, _, _, ss)) =
      if Substring.size ss = 0 then
        z
      else
        let
          val (s, loc) = L.any src
        in
          raise Parse { expected = Dec, found = Invalid s, at = loc }
        end
  in
    fun parse { fileName, lineOffset } s =
      let
        val src = (fileName, lineOffset + 1, 0, Substring.full s)
      in
        case (star dec o L.drop) src of
          NONE => check ([], src)
        | SOME (r, _, src) => check (r, src)
      end
  end
end
