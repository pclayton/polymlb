let
  open OS.FileSys OS.Path PolyML

  val baseDir =
    case getUseFileName () of
      NONE => raise Fail "Invalid usage; import with use"
    | SOME d => dir d

  val (strNames, sigNames, funNames) =
    let
      fun doDir (d, p, q, strs, sigs, funs) =
        case readDir d of
          NONE =>
            (case q of
              [] => (strs, sigs, funs)
            | p'::q =>
                let
                  val (d, p) = (openDir p', p') handle _ => (d, p)
                in
                  doDir (d, p, q, strs, sigs, funs)
                end)
        | SOME e => doDir
            (if (isDir o concat) (p, e) then
              (d, p, concat (p, e)::q, e::strs, sigs, funs)
            else
              case splitBaseExt e of
                { base = "PolyMLB", ... } => (d, p, q, strs, sigs, funs)
              | { base = "build",   ... } => (d, p, q, strs, sigs, funs)
              | { base = "ml_bind", ... } => (d, p, q, strs, sigs, funs)
              | { base, ext = SOME "sml" } => (d, p, q, base::strs, sigs, funs)
              | { base, ext = SOME "sig" } => (d, p, q, strs, base::sigs, funs)
              | { base, ext = SOME "fun" } => (d, p, q, strs, sigs, base::funs)
              | _ => (d, p, q, strs, sigs, funs))
    in
      doDir
        ( openDir (if baseDir = "" then "." else baseDir)
        , baseDir, [], [], [], []
        )
    end
    handle e => raise Fail ("Could not fetch files, got: " ^ exnMessage e)

  fun getOld f = List.mapPartial
    (fn s => case f globalNameSpace s of SOME z => SOME (s, z) | NONE => NONE)

  val oldStrs = getOld #lookupStruct strNames
  val oldSigs = getOld #lookupSig    sigNames
  val oldFuns = getOld #lookupFunct  funNames

  val oldSuffixes = !suffixes

  val err : exn option ref = ref NONE
in
  (* "" is necessary to match directories *)
  suffixes := ["", ".sml", ".sig", ".fun"];
  (make o concat) (baseDir, "PolyMLB") handle e => err := SOME e;
  (* forget top level declarations *)
  app (fn (f, l) => app f l)
    [ (Compiler.forgetStructure, strNames)
    , (Compiler.forgetSignature, sigNames)
    , (Compiler.forgetFunctor,   funNames)
    ];
  (* restore old env *)
  app (#enterStruct globalNameSpace) oldStrs;
  app (#enterSig    globalNameSpace) oldSigs;
  app (#enterFunct  globalNameSpace) oldFuns;
  suffixes := oldSuffixes;
  case !err of SOME e => Exception.reraise e | _ => ()
end
