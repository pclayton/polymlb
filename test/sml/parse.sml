structure L = PolyMLB.Lex
structure P = PolyMLB.Parse
structure E = P.Element

datatype z = datatype P.dec_kind
datatype z = datatype P.exp_kind

fun mapi f l =
  let
    fun mapi' (i, []) = []
      | mapi' (i, x::xs) = f (x, i) :: mapi' (i + 1, xs)
  in
    mapi' (1, l)
  end

fun pos i =
  L.makePos { startLine = i, startCol = i, endLine = i, endCol = i }

fun t (tk, i) = (tk, pos i)

fun p (z : 'a, i, j) : 'a * PolyML.location =
  ( z
  , { file = "", startLine = i, startPosition = i, endLine = j, endPosition = j }
  )

fun p' (z, i) = p (z, i, i)

fun ` l = map p l
fun `` l = map p' l;

app
  (fn (n, l, r) =>
    "Parse.parse parses " ^ n ^ " declarations correctly"
    assert P.parse "" (mapi t l, { start = pos 0, eof = pos (length l) }) eq r)
  [ ( "annotation"
    , [L.Ann, L.String "foo", L.String "bar", L.In, L.End]
    , `[(Ann (["foo", "bar"], []), 1, 5)]
    )
  , ( "single basis"
    , [L.Basis, L.Symbol "foo", L.Eq, L.Symbol "bar"]
    , `[(Basis [("foo", p' (Id "bar", 4))], 1, 4)]
    )
  , ( "and basis"
    , [ L.Basis, L.Symbol "foo", L.Eq, L.Symbol "bar", L.And, L.Symbol "baz"
      , L.Eq, L.Symbol "qux"
      ]
    , `[(Basis [("foo", p' (Id "bar", 4)), ("baz", p' (Id "qux", 8))], 1, 8)]
    )
  , ( "file"
    , [L.Symbol "foo.sml", L.String "foo bar.sml"]
    , ``[(File "foo.sml", 1), (File "foo bar.sml", 2)]
    )
  , ( "empty local/in"
    , [L.Local, L.In, L.End]
    , `[(Local ([], []), 1, 3)]
    )
  , ( "local/in"
    , [L.Local, L.Symbol "foo", L.Symbol "bar", L.In, L.Symbol "baz", L.End]
    , `[(Local (``[(File "foo", 2), (File "bar", 3)], ``[(File "baz", 5)]), 1, 6)]
    )
  , ( "open"
    , [L.Open, L.Symbol "foo", L.Symbol "bar"]
    , `[(Open ["foo", "bar"], 1, 3)]
    )
  , ( "functor, signature and structure bindings"
    , [ L.Functor, L.Symbol "f1", L.Signature, L.Symbol "s1", L.Eq, L.Symbol "s2"
      , L.Structure, L.Symbol "s1", L.And, L.Symbol "s2", L.Eq, L.Symbol "s3"
      ]
    , `[ (Functor [("f1", "f1")], 1, 2), (Signature [("s1", "s2")], 3, 6)
       , (Structure [("s1", "s1"), ("s2", "s3")], 7, 12)
       ]
    )
  , ( "semi colon separated"
    , [ L.Semi, L.Semi, L.Open, L.Symbol "foo", L.Symbol "bar", L.Semi
      , L.Symbol "baz", L.Semi, L.Semi
      ]
    , `[(Open ["foo", "bar"], 3, 5), (File "baz", 7, 7)]
    )
  ];

app
  (fn (n, l, r) =>
    "Parse.parse parses " ^ n ^ " expressions correctly"
    assert
      P.parse ""
        ( mapi t ([L.Basis, L.Symbol "b", L.Eq] @ l)
        , { start = pos 0, eof = pos (length l) }
        )
    eq
      `[(Basis [("b", r)], 1, 3 + length l)])
  [ ( "empty bas"
    , [L.Bas, L.End]
    , p (Bas [], 4, 5)
    )
  , ( "bas"
    , [L.Bas, L.Symbol "foo", L.Symbol "bar", L.End]
    , p (Bas (``[(File "foo", 5), (File "bar", 6)]), 4, 7)
    )
  , ( "id"
    , [L.Symbol "foo"]
    , p' (Id "foo", 4)
    )
  , ( "empty let/in"
    , [L.Let, L.In, L.Symbol "foo", L.End]
    , p (Let ([], p' (Id "foo", 6)), 4, 7)
    )
  , ( "let/in"
    , [L.Let, L.Symbol "foo", L.Symbol "bar", L.In, L.Symbol "baz", L.End]
    , p (Let (``[(File "foo", 5), (File "bar", 6)], p' (Id "baz", 8)), 4, 9)
    )
  ]
