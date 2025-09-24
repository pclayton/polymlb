structure NameSpace :
sig
  (* A basis namespace; i.e a regular PolyML namespace augmented with bases
   * operations. Not threadsafe.
   *)
  datatype t = N of
      { allBas : unit -> (string * t) list
      , enterBas : (string * t) -> unit
      , lookupBas : string -> t option
      } * PolyML.NameSpace.nameSpace

  (* Wrapper over PolyML.globlalNameSpace; can be imported into. *)
  val global : t

  (* Read only namespace of the basis library.
   * $(SML_LIB)/basis/basis.mlb
   *)
  val basis : t

  (* Read only namespace of the PolyML extensions (PolyML, Thread, etc).
   * $(SML_LIB)/basis/poly.mlb
   *)
  val poly : t

  (* Read only namespace that contains everything as well as the basis and poly
   * namespaces under the names "BasisLib" and "PolyLib".
   *)
  val all : t

  val empty : unit -> t

  val import : { src : t, dst : t } -> unit

  (* Given a base namespace:
   * - reading from loc searches in base if no result
   * - writing to loc only writes to loc
   * - reading from pub searches in loc (which in turns searches in base)
   * - writing to pub also writes to base
   *)
  val delegates : t -> { loc : t, pub : t }
end =
struct
  structure H = HashArray
  structure L = List
  structure N = PolyML.NameSpace

  datatype t = N of
      { allBas : unit -> (string * t) list
      , enterBas : (string * t) -> unit
      , lookupBas : string -> t option
      } * PolyML.NameSpace.nameSpace

  val pgns = PolyML.globalNameSpace

  type 'a fns =
      (unit -> (string * 'a) list)
    * (string * 'a -> unit)
    * (string -> 'a option)

  fun fns () : 'a fns =
    let
      val h : 'a H.hash = H.hash 20
    in
      ( fn () => H.fold (fn (s, v, l) => (s, v)::l) [] h
      , fn (s, v) => H.update (h, s, v)
      , fn s => H.sub (h, s)
      )
    end

  val global =
    let
      val (aBas, eBas, lBas) : t fns = fns ()
    in
      N ({ allBas = aBas, lookupBas = lBas, enterBas = eBas }, pgns)
    end

  local
    (* Copy over only known basis / poly identifiers; this avoids leaking
     * whatever else may be defined at the top level at build time (e.g the
     * various PolyMLB substructures).
     *)

    (* https://smlfamily.github.io/Basis/top-level-chapter.html#section:4 *)
    val basisFixs =
      [ "*", "/", "div", "mod", "+", "-", "^", "::", "@", "=", "<>", ">", ">="
      , "<", "<=", ":=", "o", "before"
      ]

    (* https://smlfamily.github.io/Basis/overview.html#section:17 *)
    val basisFcts = ["ImperativeIO", "PrimIO", "StreamIO"]

    (* https://smlfamily.github.io/Basis/overview.html#section:12
     * https://smlfamily.github.io/Basis/overview.html#section:15
     *)
    val basisSigs =
      [ "ARRAY", "ARRAY_SLICE", "BIN_IO", "BOOL", "BYTE", "CHAR", "COMMAND_LINE"
      , "DATE", "GENERAL", "IEEE_REAL", "IMPERATIVE_IO", "INTEGER", "IO", "LIST"
      , "LIST_PAIR", "MATH", "MONO_ARRAY", "MONO_ARRAY_SLICE", "MONO_VECTOR"
      , "MONO_VECTOR_SLICE", "OPTION", "OS", "OS_FILE_SYS", "OS_IO", "OS_PATH"
      , "OS_PROCESS", "PRIM_IO" , "REAL", "STREAM_IO", "STRING", "STRING_CVT"
      , "SUBSTRING", "TEXT", "TEXT_IO", "TEXT_STREAM_IO", "TIME", "TIMER"
      , "VECTOR", "VECTOR_SLICE", "WORD"
      ] @
      [ "ARRAY2", "BIT_FLAGS", "GENERIC_SOCK", "INET_SOCK", "INT_INF"
      , "MONO_ARRAY2", "NET_HOST_DB", "NET_PROT_DB", "NET_SERV_DB", "PACK_REAL"
      , "PACK_WORD", "POSIX", "POSIX_ERROR", "POSIX_FILE_SYS", "POSIX_IO"
      , "POSIX_PROC_ENV", "POSIX_PROCESS", "POSIX_SIGNAL", "POSIX_SYS_DB"
      , "POSIX_TTY", "SOCKET", "UNIX", "UNIX_SOCK", "WINDOWS"
      ]

    (* https://smlfamily.github.io/Basis/overview.html#section:13
     * https://smlfamily.github.io/Basis/overview.html#section:16
     *)
    val basisStrs =
      let
        fun nStructs i =
          let
            val i = Int.toString i
            val int  = "Int"  ^ i
            val real = "Real" ^ i
            val word = "Word" ^ i
          in
            [ int ^ "Array", int ^ "Array2", int ^ "ArraySlice", int
            , int ^ "Vector", int ^ "VectorSlice", "PackWord" ^ i ^ "Big"
            , "PackWord" ^ i ^ "Little" , "PackReal" ^ i ^ "Big"
            , "PackReal" ^ i ^ "Little", real ^ "Array", real ^ "Array2"
            , real ^ "ArraySlice", real, real ^ "Vector", real ^ "VectorSlice"
            , word ^ "Array", word ^ "Array2" , word ^ "ArraySlice"
            , word ^ "Vector", word ^ "VectorSlice", word
            ]
          end
      in
        [ "Array", "ArraySlice", "BinIO", "BinPrimIO", "Bool", "Byte"
        , "CharArray", "CharArraySlice", "Char", "CharVector", "CharVectorSlice"
        , "CommandLine", "Date", "General", "IEEEReal", "Int", "IO", "LargeInt"
        , "LargeReal", "LargeWord", "List", "ListPair", "Math", "Option", "OS"
        , "Position", "Real", "StringCvt", "String", "Substring", "TextIO"
        , "TextPrimIO", "Text", "Timer", "Time", "VectorSlice", "Vector"
        , "Word8Array", "Word8ArraySlice", "Word8Vector", "Word8VectorSlice"
        , "Word8", "Word"
        ] @
        [ "Array2", "BoolArray", "BoolArray2", "BoolArraySlice", "BoolVector"
        , "BoolVectorSlice", "CharArray2", "FixedInt", "GenericSock", "INetSock"
        , "IntArray", "IntArray2", "IntArraySlice", "IntVector"
        , "IntVectorSlice", "IntInf", "NetHostDB", "NetProtDB", "NetServDB"
        , "PackRealBig", "PackRealLittle", "Posix", "RealArray2", "RealArray"
        , "RealArraySlice", "RealVector", "RealVectorSlice", "Socket", "SysWord"
        , "UnixSock", "Unix", "WideCharArray", "WideCharArray2"
        , "WideCharArraySlice", "WideChar", "WideCharVector"
        , "WideCharVectorSlice", "WideString", "WideSubstring", "WideTextPrimIO"
        , "WideText", "Windows"
        ] @ nStructs 8 @ nStructs 16 @ nStructs 32 @ nStructs 63 @ nStructs 64
      end

    (* https://smlfamily.github.io/Basis/top-level-chapter.html#section:2
     * https://smlfamily.github.io/Basis/top-level-chapter.html#section:3
     *)
    val basisTyps =
      [ "unit", "int", "word", "real", "char", "string", "substring", "exn"
      , "array", "vector", "ref", "bool", "option", "order", "list"
      ]

    (* https://smlfamily.github.io/Basis/top-level-chapter.html#section:2 *)
    val basisVals =
      [ "false", "true", "NONE", "SOME", "LESS", "EQUAL", "GREATER", "nil", "::"
      ] @
      [ "Bind", "Chr", "Div", "Domain", "Empty", "Fail", "Match", "Option"
      , "Overflow", "Size", "Span", "Subscript"
      ] @
      [ "!", ":=", "@", "^", "app", "before", "ceil", "chr", "concat"
      , "exnMessage", "exnName", "explode", "floor", "foldl", "foldr", "getOpt"
      , "hd", "ignore", "implode", "isSome", "length", "map", "not", "null", "o"
      , "ord", "print", "real", "ref", "rev", "round", "size", "str"
      , "substring", "tl", "trunc", "use", "valOf", "vector"
      ] @
      [ "+", "-", "*", "div", "mod", "/", "~", "abs", "<", ">", "<=", ">="
      , "<>", "="
      ]

    (* https://polyml.org/documentation/Reference/Basis.html *)
    val polySigs =
      [ "ASN1", "FOREIGN", "INET6_SOCK", "SIGNAL", "SML90", "THREAD", "WEAK"
      ]

    (* https://polyml.org/documentation/Reference/Basis.html *)
    val polyStrs =
      [ "Asn1", "Foreign", "HashArray", "INet6Sock", "Net6HostDB", "PolyML"
      , "RunCall" ,"Signal", "SingleAssignment", "SML90", "Thread", "ThreadLib"
      , "Universal", "UniversalArray", "Weak"
      ]

    local

      fun copy f l : 'a fns =
        let
          val h : 'a H.hash = H.hash (length l * 5 div 4)
          fun cp f m k = case f k of SOME v => H.update (m, k, v) | NONE => ()
        in
          app (cp f h) l;
          ( fn () => H.fold (fn (s, v, l) => (s, v)::l) [] h
          , fn _ => ()
          , fn s => H.sub (h, s)
          )
        end

      fun fromList l : 'a fns =
        let
          val h : 'a H.hash = H.hash (length l * 5 div 4)
        in
          app (fn (k, v) => H.update (h, k, v)) l;
          ( fn () => H.fold (fn (s, v, l) => (s, v)::l) [] h
          , fn _ => ()
          , fn s => H.sub (h, s)
          )
        end
    in
      fun new (lBas, lFix, lFun, lSig, lStr, lTyp, lVal) =
        let
          val (aBas, eBas, lBas) = fromList lBas
          val (aFix, eFix, lFix) = copy (#lookupFix    pgns) lFix
          val (aFun, eFun, lFun) = copy (#lookupFunct  pgns) lFun
          val (aSig, eSig, lSig) = copy (#lookupSig    pgns) lSig
          val (aStr, eStr, lStr) = copy (#lookupStruct pgns) lStr
          val (aTyp, eTyp, lTyp) = copy (#lookupType   pgns) lTyp
          val (aVal, eVal, lVal) = copy (#lookupVal    pgns) lVal
        in
           N (
                { allBas    = aBas, enterBas    = eBas, lookupBas    = lBas
                }
              , { allFix    = aFix, enterFix    = eFix, lookupFix    = lFix
                , allFunct  = aFun, enterFunct  = eFun, lookupFunct  = lFun
                , allSig    = aSig, enterSig    = eSig, lookupSig    = lSig
                , allStruct = aStr, enterStruct = eStr, lookupStruct = lStr
                , allType   = aTyp, enterType   = eTyp, lookupType   = lTyp
                , allVal    = aVal, enterVal    = eVal, lookupVal    = lVal
                }
              )
          end
    end

    val basis =
      new ([], basisFixs, basisFcts, basisSigs, basisStrs, basisTyps, basisVals)

    val poly = new ([], [], [], polySigs, polyStrs, [], [])
  in
    val basis = basis
    val poly  = poly
    val all =
      new
        ( [("BasisLib", basis), ("PolyLib", poly)], basisFixs, basisFcts
        , basisSigs @ polySigs, basisStrs @ polyStrs, basisTyps, basisVals
        )
  end

  fun empty () =
    let
      val (aBas, eBas, lBas) : t fns                         = fns ()
      val (aFix, eFix, lFix) : N.Infixes.fixity fns          = fns ()
      val (aFun, eFun, lFun) : N.Functors.functorVal fns     = fns ()
      val (aSig, eSig, lSig) : N.Signatures.signatureVal fns = fns ()
      val (aStr, eStr, lStr) : N.Structures.structureVal fns = fns ()
      val (aTyp, eTyp, lTyp) : N.TypeConstrs.typeConstr fns  = fns ()
      val (aVal, eVal, lVal) : N.Values.value fns            = fns ()
    in
      N (
          { allBas    = aBas, enterBas    = eBas, lookupBas    = lBas
          }
        , { allFix    = aFix, enterFix    = eFix, lookupFix    = lFix
          , allFunct  = aFun, enterFunct  = eFun, lookupFunct  = lFun
          , allSig    = aSig, enterSig    = eSig, lookupSig    = lSig
          , allStruct = aStr, enterStruct = eStr, lookupStruct = lStr
          , allType   = aTyp, enterType   = eTyp, lookupType   = lTyp
          , allVal    = aVal, enterVal    = eVal, lookupVal    = lVal
          }
        )
      end

  fun import { src = N (bs1, ns1), dst = N (bs2, ns2) } =
    ( app (#enterBas    bs2) (#allBas    bs1 ())
    ; app (#enterFix    ns2) (#allFix    ns1 ())
    ; app (#enterFunct  ns2) (#allFunct  ns1 ())
    ; app (#enterSig    ns2) (#allSig    ns1 ())
    ; app (#enterStruct ns2) (#allStruct ns1 ())
    ; app (#enterType   ns2) (#allType   ns1 ())
    ; app (#enterVal    ns2) (#allVal    ns1 ())
    )

  local
    (* lookup delegate, enter delegate, whether to propagate enter *)
    (* todo: delegate { lookup : t, enter : t option } *)
    fun delegate (N (bs, ns), N (bs', ns'), enter) =
      let
        fun fns (all, enter, lookup) : 'a fns =
          let
            val h : 'a H.hash = H.hash 20
          in
            ( fn () => H.fold (fn (s, v, l) => (s, v)::l) (all ()) h
            , case enter of
                NONE => (fn (s, v) => H.update (h, s, v))
              | SOME f => (fn (s, v) => (H.update (h, s, v); f (s, v)))
            , fn s => case H.sub (h, s) of NONE => lookup s | z => z
            )
          end
        fun e f = if enter then SOME f else NONE
        val (aBas, eBas, lBas) = fns (#allBas bs,    e (#enterBas bs'),    #lookupBas bs)
        val (aFix, eFix, lFix) = fns (#allFix ns,    e (#enterFix ns'),    #lookupFix ns)
        val (aFun, eFun, lFun) = fns (#allFunct ns,  e (#enterFunct ns'),  #lookupFunct ns)
        val (aSig, eSig, lSig) = fns (#allSig ns,    e (#enterSig ns'),    #lookupSig ns)
        val (aStr, eStr, lStr) = fns (#allStruct ns, e (#enterStruct ns'), #lookupStruct ns)
        val (aTyp, eTyp, lTyp) = fns (#allType ns,   e (#enterType ns'),   #lookupType ns)
        val (aVal, eVal, lVal) = fns (#allVal ns,    e (#enterVal ns'),    #lookupVal ns)
      in
         N (
              { allBas    = aBas, enterBas    = eBas, lookupBas    = lBas
              }
            , { allFix    = aFix, enterFix    = eFix, lookupFix    = lFix
              , allFunct  = aFun, enterFunct  = eFun, lookupFunct  = lFun
              , allSig    = aSig, enterSig    = eSig, lookupSig    = lSig
              , allStruct = aStr, enterStruct = eStr, lookupStruct = lStr
              , allType   = aTyp, enterType   = eTyp, lookupType   = lTyp
              , allVal    = aVal, enterVal    = eVal, lookupVal    = lVal
              }
            )
      end
  in
    fun delegates ns =
      let
        val loc = delegate (ns, ns, false)
      in
        { loc = loc, pub = delegate (loc, ns, true) }
      end
  end
end
