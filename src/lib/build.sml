case PolyML.getUseFileName () of
  NONE => raise Fail "invalid usage; import with use\n"
| SOME d => PolyML.make (OS.Path.dir d ^ "/PolyMLB");

PolyML.Compiler.forgetSignature "QUEUE";

app PolyML.Compiler.forgetFunctor [ "FifoFn", "PrioFn", "TPFn" ];

app PolyML.Compiler.forgetStructure
  [ "Ann"
  , "Basis"
  , "Compile"
  , "Dag"
  , "Lex"
  , "Log"
  , "NameSpace"
  , "Parse"
  , "Path"
  , "ThreadPool"
  ]
