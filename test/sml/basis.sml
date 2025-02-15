structure A = PolyMLB.Ann
structure B = PolyMLB.Basis
structure H = HashArray
structure P = PolyMLB.Parse

val pmap : string H.hash = H.hash 1

val loc =
  { file = ""
  , startLine = 0, startPosition = 0
  , endLine = 0, endPosition = 0
  }

fun fromParse p () = B.fromParse
  { disabledAnns = [], pathMap = pmap, path = "/", exts = NONE, logger = NONE }
  p;

"Basis.fromParse raises on duplicate bind"
assert
  fromParse [(P.Structure [("foo", "bar"), ("foo", "baz")], loc)]
raisesExact
  B.Validation (B.DuplicateBind, "foo", loc);

"Basis.fromParse raises on invalid file extension"
assert
  fromParse [(P.File "foo.bar", loc)]
raisesExact
  B.Validation (B.Extension, "foo.bar", loc);

"Basis.fromParse raises on missing file extension"
assert
  fromParse [(P.File "foo", loc)]
raisesExact
  B.Validation (B.Extension, "foo", loc);

"Basis.fromParse raises on unbound path variable"
assert
  fromParse [(P.File "$(FOO).sml", loc)]
raisesExact
  B.Validation (B.UnboundVariable, "FOO", loc);

fun fromParse p = B.fromParse
  { disabledAnns = [], pathMap = pmap, path = "/", exts = NONE, logger = NONE }
  p;

"Basis.fromParse valid"
assert
  fromParse
    [ (P.Local
        ( [(P.Basis [("b1", (P.Id "b2", loc))], loc)]
        , [(P.File "foo.sml", loc), (P.Functor [("f1", "f2")], loc)]
        ), loc)
    , (P.File "bar.mlb", loc)
    ]
eq
  [ B.Local
      ( [B.Basis ("b1", B.Id "b2")]
      , [B.SourceFile "/foo.sml", B.Functor ("f1", "f2")]
      )
  , B.BasisFile "/bar.mlb"
  ];

"Basis.fromParse removes unknown annotations"
assert
  fromParse [(P.Ann (["foo", "debug true", "bar:baz"], []), loc)]
eq
  [B.Ann ([A.Debug true], [])];

"Basis.fromParse removes Discard annotated declarations"
assert
  fromParse [(P.Ann (["discard"], [(P.File "foo.sml", loc)]), loc)]
eq
  [];

"Basis.fromParse removes files matching an enclosing IgnoreFiles"
assert
  fromParse
    [(P.Ann
      ( ["ignoreFiles foo.sml"]
      , [(P.File "foo.sml", loc), (P.File "dir/foo.sml", loc)]
      ), loc)]
eq
  [B.Ann ([A.IgnoreFiles ["foo.sml"]], [])];

"Basis.fromParse splits Open declarations"
assert
  fromParse [(P.Open ["s1", "s2"], loc)]
eq
  [B.Open "s1", B.Open "s2"];

"Basis.fromParse splits and bindings"
assert
  fromParse [(P.Structure [("s1", "s1"), ("s2", "s2")], loc)]
eq
  [B.Structure ("s1", "s1"), B.Structure ("s2", "s2")];

fun fromParse l p = B.fromParse
  { disabledAnns = l, pathMap = pmap, path = "/", exts = NONE, logger = NONE }
  p;

"Basis.fromParse removes disabled annotations"
assert
  fromParse [A.Debug true] [(P.Ann (["debug true"], []), loc)]
eq
  []
