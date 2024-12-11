structure H = HashArray
structure P = PolyMLB.Path

val vars : string H.hash = H.hash 10;
app (fn (k, v) => H.update (vars, k, v))
  [ ("foo", "bar")
  , ("var1", "$(var2)")
  , ("var2", "var3")
  , ("empty", "")
  ];

val p = P.process vars;

"Path.process substitutes simple variables"
assert p "a $(foo) b" eq P.Path "a bar b";

"Path.process substitutes recursive and empty variables"
assert p "$(var1) $(empty)." eq P.Path "var3 .";

"Path.process fails on first unbound variable"
assert p "$(unset) $(unset2)" eq P.Unbound "unset";

"Path.process fails on nameless variables"
assert p "$()" eq P.Unbound ""
