val SML_LIB =
  case OS.Process.getEnv "SML_LIB" of
    SOME v => v
  | NONE => raise Fail "missing SML_LIB"

val (libDir, binDir) =
  case PolyML.getUseFileName () of
    NONE => (print "invalid usage; import with use\n"; raise Fail "")
  | SOME d =>
      let
        open OS.Path
        val usePath = OS.FileSys.fullPath (dir d)
        val cwd = OS.FileSys.getDir ()
        val binDir = mkRelative { path = usePath, relativeTo = cwd }
        val libDir = (mkCanonical o concat) (binDir, "../lib")
      in
        (libDir, binDir)
      end

fun eval s =
  let
    val str = TextIO.openString s
  in
    PolyML.compiler (fn () => TextIO.input1 str, []) ()
  end;

if OS.Path.isRelative SML_LIB then
  TextIO.output (TextIO.stdErr, "warning: relative SML_LIB (" ^ SML_LIB ^ ")\n")
else
  ();

PolyML.use (libDir ^ "/build.sml");

eval ("HashArray.update (PolyMLB.pathMap, \"SML_LIB\", \"" ^ SML_LIB ^ "\")");

use (binDir ^ "/version.sml");
use (binDir ^ "/main.sml")
