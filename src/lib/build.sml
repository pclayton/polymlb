case PolyML.getUseFileName () of
  NONE => raise Fail "invalid usage; import with use\n"
| SOME d => PolyML.make (OS.Path.dir d ^ "/PolyMLB");

PolyML.Compiler.forgetSignature "QUEUE";
PolyML.Compiler.forgetFunctor "TP";

app PolyML.Compiler.forgetStructure
  [ "Ann"
  , "Basis"
  , "Compile"
  , "Dag"
  , "Fifo"
  , "Lex"
  , "Log"
  , "NameSpace"
  , "Prio"
  , "Parse"
  , "Path"
  , "ThreadPool"
  ]
