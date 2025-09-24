structure Lex :>
sig
  datatype token =
    String of string
  | Symbol of string
  | And
  | Ann
  | Bas
  | Basis
  | End
  | Eq
  | Functor
  | In
  | Let
  | Local
  | Open
  | Semi
  | Signature
  | Structure

  val toString : token -> string

  eqtype position

  val makePos : { startLine : int, startCol : int, endLine : int, endCol : int }
    -> position
  val joinPos : position * position -> position
  val toPolyLoc : string * position -> PolyML.location

  type t = (token * position) list * { start : position, eof : position }

  datatype err_kind =
    BadChar of char
  | BadWord of string
  | UnclosedComment
  | UnclosedString

  type err = err_kind * PolyML.location

  exception Lex of err

  val errToString : (string -> string) -> err -> string

  (* raises Lex in case of
   * - unclosed comment
   * - unclosed string
   * - invalid string char
   * - bad reserved word
   *)
  val lex : string -> string -> t
end =
struct
  structure C  = Char
  structure S  = String
  structure SS = Substring
  structure V  = Vector

  datatype token =
    String of string
  | Symbol of string
  | And
  | Ann
  | Bas
  | Basis
  | End
  | Eq
  | Functor
  | In
  | Let
  | Local
  | Open
  | Semi
  | Signature
  | Structure

  fun toString t =
    case t of
      String s => "string \"" ^ s ^ "\""
    | Symbol s => "symbol \"" ^ s ^ "\""
    | And => "and"
    | Ann => "ann"
    | Bas => "bas"
    | Basis => "basis"
    | End => "end"
    | Eq => "="
    | Functor => "functor"
    | In => "in"
    | Let => "let"
    | Local => "local"
    | Open => "open"
    | Semi => ";"
    | Signature => "signature"
    | Structure => "structure"

  (* Positions are packed in a single 64 bit word, where each line or column is
   * represented with 16 bit. This assumes that {files,columns} are no longer
   * than 65536 {lines,characters}. The storing order is [start line, start col,
   * end line, end col], from the msb to the lsb.
   *)
  type position = Word64.word

  local
    val `& = Word64.andb
    val `| = Word64.orb
    val << = Word64.<<
    val >> = Word64.>>

    infix 8 `& `| `^ << >>

    val toi = Word64.toInt
    val tow = Word64.fromInt

    val max = toi (0w1 << 0w16)

    fun chkdw i =
      if i >= max then
        raise Fail ("Lex.mkPos: too large: " ^ Int.toString i)
      else
        tow i

    val slineMask = (0w1 << 0w16 - 0w1) << 0w48
    val scolMask  = (0w1 << 0w16 - 0w1) << 0w32
    val elineMask = (0w1 << 0w16 - 0w1) << 0w16
    val ecolMask  =  0w1 << 0w16 - 0w1

    val sMask = (0w1 << 0w32 - 0w1) << 0w32
    val eMask =  0w1 << 0w32 - 0w1
  in
    fun mkPos (l, c, l', c') =
      (chkdw l << 0w48) `| (chkdw c << 0w32) `| (chkdw l' << 0w16) `| chkdw c'

    fun joinPos (w1, w2) =
         Word64.min (w1 `& sMask, w2 `& sMask)
      `| Word64.max (w1 `& eMask, w2 `& eMask)

    fun unpackStart w =
      (toi ((w `& slineMask) >> 0w48), toi ((w `& scolMask) >> 0w32))

    fun toPolyLoc (s, w) =
      { file = s
      , startLine     = toi ((w `& slineMask) >> 0w48)
      , startPosition = toi ((w `& scolMask)  >> 0w32)
      , endLine       = toi ((w `& elineMask) >> 0w16)
      , endPosition   = toi  (w `& ecolMask)
      }
  end

  fun makePos { startLine, startCol, endLine, endCol } =
      mkPos (startLine, startCol, endLine, endCol)

  fun polyLoc (s, l, c, l', c') =
    { file = s
    , startLine = l, startPosition = c
    , endLine = l', endPosition = c'
    }

  datatype err_kind =
    BadChar of char
  | BadWord of string
  | UnclosedComment
  | UnclosedString

  type err = err_kind * PolyML.location

  exception Lex of err

  fun errToString fmt (e, at) =
    concat
      [ Log.locFmt fmt at
      , ": error: invalid token: "
      , case e of
          BadChar c => "bad character '" ^ Char.toString c ^ "'"
        | BadWord w => "reserved word not allowed here '" ^ w ^ "'"
        | UnclosedComment => "unclosed comment"
        | UnclosedString => "unclosed string"
      ]

  type t = (token * position) list * { start : position, eof : position }

  fun skip { name, line, col, src, res } =
    let
      fun ok i = i < SS.size src
      fun sub i = SS.sub (src, i)

      fun comment (s, l, c, i) =
        if not (ok i) then
          let
            val (l', c') = hd s
          in
            raise Lex (UnclosedComment, polyLoc (name, l', c', l, c))
          end
        else
          case sub i of
            #"(" =>
              if ok (i + 1) andalso sub (i + 1) = #"*" then
                comment ((l, c)::s, l, c + 2, i + 2)
              else
                comment (s, l, c + 1, i + 1)
          | #"*" =>
              if ok (i + 1) andalso sub (i + 1) = #")" then
                case s of
                  [] => raise Fail "Lex.skip.comment: impossible"
                | [_] => (l, c + 2, i + 2)
                | _::s => comment (s, l, c + 2, i + 2)
              else
                comment (s, l, c + 1, i + 1)
          | #"\n" => comment (s, l + 1, 1, i + 1)
          |   _   => comment (s, l, c + 1, i + 1)

      fun skip' (l, c, i) =
        if not (ok i) then
          (l, c, i)
        else if sub i = #"\n" then
          skip' (l + 1, 0, i + 1)
        else if C.isSpace (sub i) then
          skip' (l, c + 1, i + 1)
        else if sub i = #"(" andalso ok (i + 1) andalso sub (i + 1) = #"*" then
          (skip' o comment) ([(l, c)], l, c + 2, i + 2)
        else
          (l, c, i)

      val (line, col, ofs) = skip' (line, col, 0)
    in
      { line = line, col = col, src = SS.triml ofs src, res = res }
    end

  local
    val S = SS.full
  in
    val keywords = (V.fromList o map (fn t => (S (toString t), t)))
      [ And, Ann, Bas, Basis, End, Eq, Functor, In, Let, Local, Open, Semi
      , Signature, Structure
      ]

    val badSymbols = (V.fromList o map S)
      [ "abstype", "andalso", "as", "case", "datatype", "do", "else"
      , "exception", "fn", "fun", "handle" , "if", "infix", "infixr", "nonfix"
      , "of", "op", "orelse", "raise", "rec", "sig", "struct", "then", "type"
      , "val", "with", "withtype", "while"
      ]
  end

  fun ssEq a b = SS.size a = SS.size b andalso SS.compare (a, b) = EQUAL

  fun reserved w =
    let
      fun find i =
        if i = V.length keywords then
          NONE
        else
          let
            val (kw, t) = V.sub (keywords, i)
          in
            if ssEq w kw then SOME t else find (i + 1)
          end
    in
      find 0
    end

  fun string (name, line, col, src) =
    let
      val sz = SS.size src

      fun read (l, c, i) =
        if i = sz then
          NONE
        else if SS.sub (src, i) = #"\n" then
          SOME (SS.sub (src, i), (l + 1, 1, i + 1))
        else
          SOME (SS.sub (src, i), (l, c + 1, i + 1))

      fun scan (l, c, i, res) =
        if i = sz then
          raise Lex (UnclosedString, polyLoc (name, line, col, l, c - 1))
        else if SS.sub (src, i) = #"\"" then
          (l, c + 1, i + 1, res)
        else
          case C.scan read (l, c, i) of
            SOME (ch, (l, c, i)) => scan (l, c, i, ch::res)
          | NONE =>
              raise Lex
                (if SS.sub (src, i) = #"\n" then
                  (UnclosedString, polyLoc (name, line, col, l, c))
                else
                  ((BadChar o SS.sub) (src, i), polyLoc (name, l, c, l, c + 1)))

      val (l, c, i, res) = scan (line, col + 1, 0, [])
    in
      (implode (rev res), l, c, SS.triml i src)
    end

  val isGoodChar =
    fn #"$" => true
     | #"(" => true
     | #")" => true
     | #"." => true
     | #"/" => true
     | #"-" => true
     | #"_" => true
     | #"'" => true
     |   c  => Char.isAlphaNum c

  fun word (col, src) =
   let
      fun f i =
        if i < SS.size src andalso (isGoodChar o SS.sub) (src, i) then
          f (i + 1)
        else
          i

      val i =
        if SS.sub (src, 0) = #"=" orelse SS.sub (src, 0) = #";" then
          1
        else
          f 0
      val (w, rest) = SS.splitAt (src, i)
    in
      (w, col + i, rest)
    end

  fun tokenize (state as { name, ... }) =
    let
      val { line, col, src, res } = skip state
    in
      if SS.size src = 0 then
        let
          val l = rev res
          val eof = mkPos (line, col, line, col)
          val start = case l of
            [] => eof
          | (_, p)::_ =>
              let
                val (l, c) = unpackStart p
              in
                mkPos (l, c, l, c)
              end
        in
          (l, { start = start, eof = eof })
        end
      else if SS.sub (src, 0) = #"\"" then
        let
          val (w, l, c, src) = string (name, line, col, SS.triml 1 src)
          val loc = mkPos (line, col, l, c)
        in
          tokenize
            { name = name
            , line = l
            , col = c
            , src = src
            , res = (String w, loc) :: res
            }
        end
      else
        let
          val (w, c, src) = word (col, src)
          val l = line
          val loc = mkPos (line, col, l, c)
        in
          if SS.size w = 0 then
            raise Lex
              ((BadChar o SS.sub) (src, 0), polyLoc (name, l, c, l, c + 1))
          else
            ();
          case reserved w of
            SOME t =>
              tokenize
                { name = name
                , line = l
                , col = c
                , src = src
                , res = (t, loc)::res
                }
          | NONE =>
              if V.exists (ssEq w) badSymbols then
                raise Lex (BadWord (SS.string w), toPolyLoc (name, loc))
              else
                tokenize
                  { name = name
                  , line = l
                  , col = c
                  , src = src
                  , res = (Symbol (SS.string w), loc) :: res
                  }
        end
    end

  fun lex n s =
    tokenize { name = n, line = 1, col = 1, src = SS.full s, res = [] }
end
