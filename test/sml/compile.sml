structure C = PolyMLB.Compile
structure N = PolyML.NameSpace

datatype z = datatype PolyMLB.Basis.dec

local
  val log = { pathFmt = fn s => s, print = fn _ => () }
in
  fun compile b () = C.compile
    log { copts = [], depsFirst = false, jobs = 0 }
    (PolyMLB.Dag.process
      log
      (fn "root" => [Ann ([PolyMLB.Ann.ImportAll], b)]
        | _ => raise Fail "")
      "root")
end;

"Compile.compile raises on invalid sml source"
assert
  compile [SourceFile "sml/source/illegal.sml"]
raisesExact (* ... *)
  (C.Compile o C.Compilation)
    ( "Pattern and expression have incompatible types.\n\
      \   Pattern: i : int : int\n\
      \   Expression: \"foo\" : string\n\
      \   Reason:\n\
      \      Can't unify int (*In Basis*) with string (*In Basis*)\n\
      \         (Different type constructors)\n"
    , { file = "sml/source/illegal.sml"
      , startLine = 1, startPosition = 0
      , endLine = 1, endPosition = 0
      }
    );

"Compile.compile re-raises execution exns"
assert
  compile [SourceFile "sml/source/bad-exec.sml"]
raisesExact
  (C.Compile o C.Execution) ("sml/source/bad-exec.sml", Fail "raised");

"Compile.compile raises on invalid bind"
assert
  compile [Structure ("S1", "S2")]
raisesExact
  C.Compile (C.UnboundId "S2");

local
  val log = { pathFmt = fn s => s, print = fn _ => () }
in
  fun compile b = C.compile
    log { copts = [], depsFirst = false, jobs = 0 }
    (PolyMLB.Dag.process
      log
      (fn "root" => [Ann ([PolyMLB.Ann.ImportAll], b)]
        | _ => raise Fail "")
      "root")
end;

"Compile.compile valid"
assert
  compile [SourceFile "sml/source/good.sml", Signature ("SIG2", "SIG")]
is
  (fn (PolyMLB.NameSpace.N (_, ns)) =>
    PolyML.pointerEq (valOf (#lookupSig ns "SIG2"), valOf (#lookupSig ns "SIG"))
    andalso
    case #lookupStruct ns "Str" of
      NONE => false
    | SOME s => Option.isSome (#lookupVal (N.Structures.contents s) "i"))
