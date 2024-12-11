structure N = PolyMLB.NameSpace
structure O = Option

val == = PolyML.pointerEq

val ns = N.empty ()
val { loc, pub } = N.delegates ns

val pns  : PolyML.NameSpace.nameSpace = case ns  of N.N (_, ns) => ns
val ploc : PolyML.NameSpace.nameSpace = case loc of N.N (_, ns) => ns
val ppub : PolyML.NameSpace.nameSpace = case pub of N.N (_, ns) => ns

val n1 = "app"
val n2 = "map"
val n3 = "rev"

infix 3 lookup
fun ns lookup s = valOf (#lookupVal ns s)

val v1 = PolyML.globalNameSpace lookup n1
val v2 = PolyML.globalNameSpace lookup n2
val v3 = PolyML.globalNameSpace lookup n3;

#enterVal pns (n1, v2);

"NameSpace enterVal"
assert pns lookup n1 matches (==, v2);

#enterVal pns (n1, v1);

"NameSpace enterVal replaces existing"
assert pns lookup n1 matches (==, v1);

"NameSpace.delegates loc reads from original"
assert ploc lookup n1 matches (==, v1);

"NameSpace.delegates pub reads from original"
assert ppub lookup n1 matches (==, v1);

#enterVal ploc (n2, v2);

"NameSpace.delegates loc write does not propagate to original"
assert #lookupVal pns n2 is not o O.isSome;

"NameSpace.delegates pub reads from loc"
assert ppub lookup n2 matches (==, v2);

#enterVal ppub (n3, v3);

"NameSpace.delegates pub write propagates to original"
assert pns lookup n3 matches (==, v3);

"NameSpace.delegates pub write propagates to loc through original"
assert ploc lookup n3 matches (==, v3);

#enterVal ploc (n3, v2);

"NameSpace.delegates loc reads from itself before original"
assert ploc lookup n3 matches (==, v2);

"NameSpace.delegates pub reads from itself before loc"
assert ppub lookup n3 matches (==, v3)
