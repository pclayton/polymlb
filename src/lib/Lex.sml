structure Lex :
sig
  structure Token :
  sig
    datatype id = Id | LongId

    datatype reserved =
      Eq
    | Semi
    | And
    | Ann
    | Bas
    | Basis
    | End
    | Functor
    | In
    | Let
    | Local
    | Open
    | Signature
    | Structure
  end

  type state = string * int * int * substring
  type 'a res = ('a * PolyML.location * state) option

  val path : state -> string res
  val id : Token.id -> state -> string res
  val ann : state -> string res
  val res : Token.reserved -> state -> unit res

  (* drop whitespace and comments *)
  val drop : state -> state

  val any : state -> string * PolyML.location
end =
struct
  structure C  = Char
  structure CV = CharVector
  structure S  = String
  structure SS = Substring

  structure Token =
  struct
    datatype id = Id | LongId

    datatype reserved =
      Eq
    | Semi
    | And
    | Ann
    | Bas
    | Basis
    | End
    | Functor
    | In
    | Let
    | Local
    | Open
    | Signature
    | Structure
  end

  open Token

  type state = string * int * int * substring
  type 'a res = ('a * PolyML.location * state) option

  fun mkLoc (f, l, c, l', c') =
    { file = f
    , startLine = l, endLine = l'
    , startPosition = c, endPosition = c'
    }

  fun drop (s, l, c, ss) =
    let
      fun sz i = i < SS.size ss
      fun sub i = SS.sub (ss, i)

      fun comment (d, l, c, i) =
        if not (sz i) then
          (l, c, i) (* todo: unclosed comment *)
        else
          case sub i of
            #"(" =>
              if sz (i + 1) andalso sub (i + 1) = #"*" then
                comment (d + 1, l, c + 2, i + 2)
              else
                comment (d, l, c + 1, i + 1)
          | #"*" =>
              if sz (i + 1) andalso sub (i + 1) = #")" then
                if d = 0 then
                  (l, c + 2, i + 2)
                else
                  comment (d - 1, l, c + 2, i + 2)
              else
                comment (d, l, c + 1, i + 1)
          | #"\n" => comment (d, l + 1, 0, i + 1)
          | _     => comment (d, l, c + 1, i + 1)

      fun f (l, c, i) =
        if not (sz i) then
          (s, l, c, SS.full "")
        else if sub i = #"\n" then
          f (l + 1, 0, i + 1)
        else if C.isSpace (sub i) then
          f (l, c + 1, i + 1)
        else if sub i = #"(" andalso sz (i + 1) andalso sub (i + 1) = #"*" then
          (f o comment) (0, l, c + 2, i + 2)
        else
          (s, l, c, SS.triml i ss)
    in
      f (l, c, 0)
    end

  fun any (s, l, c, ss) =
    let
      val ss' = SS.string (SS.takel (not o C.isSpace) ss)
    in
      (ss', mkLoc (s, l, c, l, c + S.size ss'))
    end

  local
    val res : unit HashArray.hash = HashArray.hash 35
    val _ = List.app (fn s => HashArray.update (res, s, ()))
      [ "abstype", "and", "andalso", "ann", "as", "bas", "basis", "case"
      , "datatype", "do", "else", "end", "exception", "fn", "fun", "functor"
      , "handle" , "if", "in", "infix", "infixr", "let", "local", "nonfix"
      , "of", "op", "open", "orelse", "raise", "rec", "sig", "signature"
      , "struct", "structure", "then", "type", "val", "with", "withtype"
      , "while"
      ]

    fun word (c, ss) =
      if SS.size ss = 0 orelse (not o C.isAlpha o SS.sub) (ss, 0) then
        NONE
      else
        let
          val (id, ss) =
            SS.splitl
              (fn c => C.isAlphaNum c orelse c = #"_" orelse c = #"'")
              ss
          val id = SS.string id
        in
          if (Option.isSome o HashArray.sub) (res, id) then
            NONE
          else
            SOME (id, c + S.size id, ss)
        end
  in
    fun id Id (s, l, c, ss) =
          (case word (c, ss) of
            NONE => NONE
          | SOME (w, c', ss) =>
              if SS.size ss > 0 andalso
                (* only chars which are valid in path or long id but not id
                 * needed for `open id+`
                 * maybe a better check would be ws or =
                 *)
                (case SS.sub (ss, 0) of
                  #"." => true | #"$" => true | #"/" => true
                | _ => false)
              then
                NONE
              else
                SOME (w, mkLoc (s, l, c, l, c'), drop (s, l, c', ss)))
      | id LongId (s, l, c, ss) =
          let
            fun f (c', ss, ws) =
              case word (c', ss) of
                NONE => NONE
              | SOME (w, c', ss) =>
                  if SS.size ss > 0 andalso SS.sub (ss, 0) = #"." then
                    f (c' + 1, SS.triml 1 ss, w::ws)
                  else
                    SOME
                      ( (S.concatWith "." o List.rev) (w::ws)
                      , mkLoc (s, l, c, l, c')
                      , drop (s, l, c', ss)
                      )
          in
            f (c, ss, [])
          end
  end

  local
    (* todo: *)
    fun string (s, l, c, ss) =
      if SS.size ss < 2 orelse SS.sub (ss, 0) <> #"\"" then
        NONE
      else
        let
          val (str, ss) = SS.splitl (fn c => c <> #"\"") (SS.triml 1 ss)
          val c' = c + SS.size str
        in
          SOME
            ( SS.string str
            , mkLoc (s, l, c, l, c')
            , drop (s, l, c', SS.triml 1 ss)
            )
        end

    (* todo: *)
    val chars = "$()./-_"
    fun simplePath (s, l, c, ss) =
      let
        val (p, ss) =
          SS.splitl
            (fn c => C.isAlphaNum c orelse CV.exists (fn c' => c = c') chars)
            ss
        fun f 0 = false
          | f i =
              case SS.sub (p, i) of
                #"." => true
              | #"/" => false
              | _ => f (i - 1)
      in
        if SS.size p = 0 orelse (not o f) (SS.size p - 1) then
          NONE
        else
          let
            val c' = c + SS.size p
          in
            SOME (SS.string p, mkLoc (s, l, c, l, c'), drop (s, l, c', ss))
          end
      end
  in
    val ann = string

    fun path s = case simplePath s of NONE => string s | z => z
  end

  fun chk w (s, l, c, ss) =
    let
      val sz = S.size w
    in
      if
        (sz = SS.size ss orelse
        (sz < SS.size ss andalso (Char.isSpace o SS.sub) (ss, sz)))
      andalso
        (SS.string o SS.slice) (ss, 0, SOME sz) = w
      then
        SOME
          ((), mkLoc (s, l, c, l, c + sz), drop (s, l, c + sz, SS.triml sz ss))
      else
        NONE
    end

  fun res r =
    chk
      (case r of
        Eq => "="
      | Semi => ";"
      | And => "and"
      | Ann => "ann"
      | Bas => "bas"
      | Basis => "basis"
      | End => "end"
      | Functor => "functor"
      | In => "in"
      | Let => "let"
      | Local => "local"
      | Open => "open"
      | Signature => "signature"
      | Structure => "structure")
end
