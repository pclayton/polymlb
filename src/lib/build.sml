val _ =
  case PolyML.getUseFileName () of
    NONE => raise Fail "invalid usage; import with use\n"
  | SOME d => PolyML.make (OS.Path.dir d ^ "/PolyMLB")

val _ = PolyML.Compiler.forgetSignature "QUEUE"
val _ = PolyML.Compiler.forgetFunctor "TP"

val _ = app PolyML.Compiler.forgetStructure
  [ "Ann"
  , "Basis"
  , "Compile"
  , "Dag"
  , "Fifo"
  , "Lex"
  , "NameSpace"
  , "Prio"
  , "Parse"
  , "Path"
  , "ThreadPool"
  ]
