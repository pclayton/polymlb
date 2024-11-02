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

  (* wrapper over PolyML.globlalNameSpace; can be imported into. *)
  val global : t

  (* read only namespace of the basis library.
   * $(SML_LIB)/basis/basis.mlb
   *)
  val basis : t

  (* read only namespace of the PolyML extensions (PolyML, Thread, etc).
   * $(SML_LIB)/basis/poly.mlb
   *)
  val poly : t

  (* read only namespace that contains everything as well as the basis and poly
   * namespaces under the names "BasisLib" and "PolyLib".
   *)
  val all : t

  val empty : unit -> t

  (* import the 2nd into the 1st *)
  val import : t * t -> unit

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

  fun fns () =
    let
      val h = H.hash 20
    in
      ( fn () => H.fold (fn (s, v, l) => (s, v)::l) [] h
      , fn (s, v) => H.update (h, s, v)
      , fn s => H.sub (h, s)
      )
    end

  fun empty () =
    let
      val (aBas, eBas, lBas) = fns ()
      val (aFix, eFix, lFix) = fns ()
      val (aFun, eFun, lFun) = fns ()
      val (aSig, eSig, lSig) = fns ()
      val (aStr, eStr, lStr) = fns ()
      val (aTyp, eTyp, lTyp) = fns ()
      val (aVal, eVal, lVal) = fns ()
    in
      N (
          { allBas       = aBas
          , enterBas     = eBas
          , lookupBas    = lBas
          }
        , { allFix       = aFix
          , allFunct     = aFun
          , allSig       = aSig
          , allStruct    = aStr
          , allType      = aTyp
          , allVal       = aVal
          , enterFix     = eFix
          , enterFunct   = eFun
          , enterSig     = eSig
          , enterStruct  = eStr
          , enterType    = eTyp
          , enterVal     = eVal
          , lookupFix    = lFix
          , lookupFunct  = lFun
          , lookupSig    = lSig
          , lookupStruct = lStr
          , lookupType   = lTyp
          , lookupVal    = lVal
          }
        )
      end

  val global =
    let
      val (aBas, eBas, lBas) = fns ()
    in
      N (
          { allBas    = aBas
          , lookupBas = lBas
          , enterBas  = eBas
          }
        , pgns
        )
    end

  local
    fun fns l =
      let
        val h = H.hash (List.length l * 5 div 4)
        val _ = L.app (fn (k, v) => H.update (h, k, v)) l
      in
        ( fn () => H.fold (fn (s, v, l) => (s, v)::l) [] h
        , fn _ => ()
        , fn s => H.sub (h, s)
        )
      end

   fun new (lBas, lFix, lFun, lSig, lStr, lTyp, lVal) =
      let
        val (aBas, eBas, lBas) = fns lBas
        val (aFix, eFix, lFix) = fns lFix
        val (aFun, eFun, lFun) = fns lFun
        val (aSig, eSig, lSig) = fns lSig
        val (aStr, eStr, lStr) = fns lStr
        val (aTyp, eTyp, lTyp) = fns lTyp
        val (aVal, eVal, lVal) = fns lVal
      in
        N (
            { allBas       = aBas
            , enterBas     = eBas
            , lookupBas    = lBas
            }
          , { allFix       = aFix
            , allFunct     = aFun
            , allSig       = aSig
            , allStruct    = aStr
            , allType      = aTyp
            , allVal       = aVal
            , enterFix     = eFix
            , enterFunct   = eFun
            , enterSig     = eSig
            , enterStruct  = eStr
            , enterType    = eTyp
            , enterVal     = eVal
            , lookupFix    = lFix
            , lookupFunct  = lFun
            , lookupSig    = lSig
            , lookupStruct = lStr
            , lookupType   = lTyp
            , lookupVal    = lVal
            }
          )
        end

    val (afix, afun, asig, astr, atyp, aval) =
      ( #allFix    pgns ()
      , #allFunct  pgns ()
      , #allSig    pgns ()
      , #allStruct pgns ()
      , #allType   pgns ()
      , #allVal    pgns ()
      )

    fun mkh l =
      let
        val h : unit H.hash = H.hash 20
      in
        h before L.app (fn s => H.update (h, s, ())) l
      end

    (* todo: find sth more resilient to extensions being added or removed *)
    val sigh = mkh ["ASN1", "FOREIGN", "SIGNAL", "SML90", "THREAD", "WEAK"]

    val strh =
      mkh
        [ "Asn1", "Foreign", "HashArray", "PolyML", "RunCall", "Signal"
        , "SingleAssignment", "SML90", "Thread", "ThreadLib", "Universal"
        , "UniversalArray", "Weak"
        ]

    fun part (h, l) =
      L.partition (fn (s, _) => (Option.isSome o H.sub) (h, s)) l

    val (psig, bsig) = part (sigh, asig)
    val (pstr, bstr) = part (strh, astr)
  in
    val basis = new ([], afix, afun, bsig, bstr, atyp, aval)
    val poly = new ([], [], [], psig, pstr, [], [])
    val all =
      new
        ( [("BasisLib", basis), ("PolyLib", poly)]
        , afix, afun, asig, astr, atyp, aval
        )
  end

  fun import (N (bs1, ns1), N (bs2, ns2)) =
    ( L.app (#enterBas bs1)    (#allBas bs2 ())
    ; L.app (#enterFix ns1)    (#allFix ns2 ())
    ; L.app (#enterFunct ns1)  (#allFunct ns2 ())
    ; L.app (#enterSig ns1)    (#allSig ns2 ())
    ; L.app (#enterStruct ns1) (#allStruct ns2 ())
    ; L.app (#enterType ns1)   (#allType ns2 ())
    ; L.app (#enterVal ns1)    (#allVal ns2 ())
    )

  local
    (* lookup delegate, enter delegate, whether to propagate enter *)
    (* todo: delegate { lookup : t, enter : t option } *)
    fun delegate (N (bs, ns), N (bs', ns'), enter) =
      let
        fun fns (all, enter, lookup) =
          let
            val h = H.hash 20
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
            { allBas       = aBas
            , enterBas     = eBas
            , lookupBas    = lBas
            }
          , { allFix       = aFix
            , allFunct     = aFun
            , allSig       = aSig
            , allStruct    = aStr
            , allType      = aTyp
            , allVal       = aVal
            , enterFix     = eFix
            , enterFunct   = eFun
            , enterSig     = eSig
            , enterStruct  = eStr
            , enterType    = eTyp
            , enterVal     = eVal
            , lookupFix    = lFix
            , lookupFunct  = lFun
            , lookupSig    = lSig
            , lookupStruct = lStr
            , lookupType   = lTyp
            , lookupVal    = lVal
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
