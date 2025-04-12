structure Parse :
sig
  (* Stringly typed version of Basis.dec and Basis.exp augmented with location.
   * withtype not allowed in signatures, see in struct for cleaner version.
   *)
  datatype dec_kind =
    Ann of string list * (dec_kind * PolyML.location) list
  | Basis of (string * (exp_kind * PolyML.location)) list
  | File of string
  | Functor of (string * string) list
  | Local of (dec_kind * PolyML.location) list * (dec_kind * PolyML.location) list
  | Open of string list
  | Signature of (string * string) list
  | Structure of (string * string) list

  and exp_kind =
    Bas of (dec_kind * PolyML.location) list
  | Id of string
  | Let of (dec_kind * PolyML.location) list * (exp_kind * PolyML.location)

  type dec = dec_kind * PolyML.location
  type exp = exp_kind * PolyML.location

  structure Element :
  sig
    datatype t =
      Dec | Ann | Basis | File | Functor | Local | Open | Signature | Structure
    | Exp | Bas | Id | Let
    | Token of Lex.token
    | EOF

    val toString : t -> string
  end

  type t = dec list

  type err =
    { expected : Element.t list
    , found : Element.t
    , at : PolyML.location
    } list

  exception Parse of err

  val parse : string -> Lex.t -> t
end =
struct
  structure L = Lex

  datatype dec_kind =
    Ann of string list * dec list
  | Basis of (string * exp) list
  | File of string
  | Functor of (string * string) list
  | Local of dec list * dec list
  | Open of string list
  | Signature of (string * string) list
  | Structure of (string * string) list

  and exp_kind =
    Bas of dec list
  | Id of string
  | Let of dec list * exp

  withtype dec = dec_kind * PolyML.location
       and exp = exp_kind * PolyML.location

  structure Element =
  struct
    datatype t =
      Dec | Ann | Basis | File | Functor | Local | Open | Signature | Structure
    | Exp | Bas | Let
    | Id
    | Token of Lex.token
    | EOF

    fun toString e =
      case e of
        Dec       => "declaration"
      | Ann       => "annotated declaration"
      | Basis     => "basis declaration"
      | File      => "file path"
      | Functor   => "functor declaration"
      | Local     => "local/in declaration"
      | Open      => "open bases"
      | Signature => "signature declaration"
      | Structure => "structure declaration"
      | Exp       => "expression"
      | Bas       => "bas expression"
      | Id        => "identifier"
      | Let       => "let/in expression"
      | Token t   => "token " ^ L.toString t
      | EOF       => "EOF"
  end

  structure E = Element

  type t = dec list

  type err =
    { expected : Element.t list
    , found : Element.t
    , at : PolyML.location
    } list

  exception Parse of err

  local
    val isIdChar =
      fn #"'" => true
       | #"_" => true
       |   c  => Char.isAlphaNum c

    (* todo: mlton only allows $() for path variables
     * see:
     * https://github.com/MLton/mlton/blob/master/mlton/front-end/mlb.lex#L190
     *)
    val isPathChar =
      fn #"$" => true
       | #"(" => true
       | #")" => true
       | #"." => true
       | #"/" => true
       | #"-" => true
       | #"_" => true
       |   c  => Char.isAlphaNum c
  in
    fun isId "" = false
      | isId s  = (Char.isAlpha o String.sub) (s, 0) andalso
          CharVector.all isIdChar s

    fun isPath "" = false
      | isPath s  = CharVector.all isPathChar s
  end

  type state = (string * L.position) * (L.token * L.position) list * L.position

  fun getPos (_, (_, l)::_, _) = l
    | getPos ((_, l), _, _) = l

  fun getPrevPos (_, _, l) = l

  (* pop next token *)
  fun ~ (z, (_, l)::ts, _) = (z, ts, l)
    | ~ s = s

  fun == (s1 : state, s2 : state) =
    case (#2 s1, #2 s2) of
      ([], []) => true
    | ((_, l1)::_, (_, l2)::_) => l1 = l2
    | _ => false

  infix ==

  datatype 'a res =
    S of state * 'a * L.position
  | F of state * (E.t list * E.t * L.position) list

  type 'a parser = state -> 'a res

  fun toExn (((n, _), _, _), l) =
    map (fn (e, f, a) => { expected = e, found = f, at = L.toPolyLoc (n, a) }) l

  fun err exp (s as ((_, p), ts, _)) =
    case ts of
      [] => F (s, [(exp, E.EOF, p)])
    | (t, p)::_ => F (s, [(exp, E.Token t, p)])

  infix 4 <|>
  infix 3 <& <&> &>
  infix 2 \ \:
  infix 1 <!>
  infixr $

  fun f $ x = f x

  (* raise on error *)
  fun (p1 <!> e) s =
    case p1 s of
      z as (S _) => z
    | F (_, l) =>
        let
          val x =
            case s of
              ((_, p), [], _) => (e, E.EOF, p)
            | (_, (t, p)::_, _) => (e, E.Token t, p)
        in
          raise (Parse o toExn) (s, x::l)
        end

  (* push error *)
  fun (p1 <|> e) s =
    case p1 s of
      z as (S _) => z
    | F (_, l) =>
        let
          val x =
            case s of
              ((_, p), [], _) => (e, E.EOF, p)
            | (_, (t, p)::_, _) => (e, E.Token t, p)
        in
          F (s, x::l)
        end

  (* discard 2nd *)
  fun (p1 <& p2) s =
    case p1 s of
      F z => F z
    | S (s, r, l) =>
        (case p2 s of
          F z => F z
        | S (s, _, l') => S (s, r, L.joinPos (l, l')))

  (* discard 1st *)
  fun (p1 &> p2) s =
    case p1 s of
      F z => F z
    | S (s, _, l) =>
        (case p2 s of
          F z => F z
        | S (s, r, l') => S (s, r, L.joinPos (l, l')))

  (* combine *)
  fun (p1 <&> p2) s =
    case p1 s of
      F z => F z
    | S (s, r, l) =>
        (case p2 s of
          F z => F z
        | S (s, r', l') => S (s, (r, r'), L.joinPos (l, l')))

  (* map *)
  fun (p \ f) s =
    case p s of
      S (s, r, l) => S (s, f r, l)
    | F z => F z

  (* map with position *)
  fun (p \: f) s =
    case p s of
      S (s, r, l) => S (s, f (r, L.toPolyLoc ((#1 o #1) s, l)), l)
    | F z => F z

  (* maybe, plus and star will propagate failures that accepted at least one
   * token, i.e all or nothing.
   *)

  fun maybe p s =
    case p s of
      S (s, r, l) => S (s, SOME r, l)
    | F z => if #1 z == s then S (s, NONE, getPrevPos s) else F z

  fun plus p s =
    let
      fun f (s', rs, l) =
        case p s' of
          S (s', r, l) => f (s', r::rs, l)
        | F z =>
            if #1 z == s' then S (s', rev rs, L.joinPos (getPos s, l)) else F z
    in
      case p s of
        S (s, r, l) => f (s, [r], l)
      | F z => F z
    end

  fun star p s =
    let
      fun f (s', rs, l) =
        case p s' of
          S (s', r, l) => f (s', r::rs, l)
        | F z =>
            if #1 z == s' then S (s', rev rs, L.joinPos (getPos s, l)) else F z
    in
      case p s of
        S (s, r, l) => f (s, [r], l)
      | F z => if #1 z == s then S (s, [], getPrevPos s) else F z
    end

  fun skip p s =
    let
      fun f s' =
        case p s' of
          S (s', _, _) => f s'
        | F z => if #1 z == s' then S (s', (), getPrevPos s) else F z
    in
      f s
    end

  fun ` kw (s as (_, (t, l)::_, _)) =
        if kw = t then S (~s, (), l) else err [E.Token kw] s
    | ` kw s = err [E.Token kw] s

  fun str s =
    case #2 s of
      (L.String r, l)::_ => S (~s, r, l)
    | _ => err [E.Token (L.String "")] s

  fun id s =
    case #2 s of
      (L.Symbol r, l)::_ => if isId r then S (~s, r, l) else err [E.Id] s
    | _ => err [E.Id] s

  fun binds kw s =
    let
      fun bind kw =
        `kw &> id <&> maybe (`L.Eq &> id)
        \ (fn (z, opt) => (z, getOpt (opt, z)))
    in
      bind kw <&> star (bind L.And) \ op:: $ s
    end

  fun basBind s =
    let
      fun bind kw = `kw &> id <& `L.Eq <&> exp
    in
      bind L.Basis <&> star (bind L.And) \ Basis o op:: <!> [E.Basis] $ s
    end

  and ann s =
    `L.Ann &> plus str <& `L.In <|> [E.Token (L.String ""), E.Token L.In]
      <&> decs L.End
    \ Ann <!> [E.Ann]
    $ s

   and file s =
    case #2 s of
      (L.String r, p)::_ => S (~s, File r, p)
    | (L.Symbol r, p)::_ =>
        if isPath r then S (~s, File r, p) else err [E.File] s
    | _ => err [E.File] s

  and funBind s = binds L.Functor \ Functor <!> [E.Functor] $ s

  and localIn s =
    `L.Local &> decs L.In <&> decs L.End \ Local <!> [E.Local] $ s

  and openBas s = `L.Open &> plus id \ Open <!> [E.Open] $ s

  and sigBind s = binds L.Signature \ Signature <!> [E.Signature] $ s

  and strBind s = binds L.Structure \ Structure <!> [E.Structure] $ s

  and dec s =
    skip (`L.Semi) &> (fn s =>
      (case #2 s of
        [] => err [E.Dec]
      | t::_ =>
          (case t of
            (L.Ann, _)       => ann
          | (L.Basis, _)     => basBind
          | (L.Functor, _)   => funBind
          | (L.Local, _)     => localIn
          | (L.Open, _)      => openBas
          | (L.Signature, _) => sigBind
          | (L.Structure, _) => strBind
          | (L.String _, _)  => file
          | (L.Symbol _, _)  => file
          | _ => fn _ => F (s, [])) <|> [E.Dec]
          \: (fn z => z))
      $ s) <& skip (`L.Semi)
    $ s

  and decs kw s = star dec <& `kw <|> [E.Dec, E.Token kw] $ s

  and basExp s = `L.Bas &> decs L.End \ Bas <!> [E.Bas] $ s

  and letIn s = `L.Let &> decs L.In <&> exp <& `L.End \ Let <!> [E.Let] $ s

  and basId s = id \ Id $ s

  and exp s =
    (case #2 s of
      [] => err [E.Exp]
    | t::_ =>
        (case t of
          (L.Bas, _)      => basExp
        | (L.Let, _)      => letIn
        | (L.Symbol _, _) => basId
        | _ => fn _ => F (s, [])) <|> [E.Exp]
        \: (fn z => z))
    $ s

  fun parse name (tokens, { start, eof }) =
    case star dec ((name, eof), tokens, start) of
      F z => raise Parse (toExn z)
    | S (s, r, _) =>
        (case #2 s of
          [] => r
        | (t, p)::_ => raise (Parse o toExn) (s, [([E.Dec], E.Token t, p)]))
end
