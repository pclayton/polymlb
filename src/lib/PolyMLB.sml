structure PolyMLB :
sig
  datatype warn =
    BadAnn of string * PolyML.location
  | NoBasisInSMLLIB of string * string (* initial value * resolved path  *)
  | RelativeSMLLIB  of string * string (* initial value * resolved value *)

  datatype opt =
    AnnDefaults of Ann.t list
  | CompileOpts of Compile.opts
    (* dummy values can be used; the anns will be completely disabled *)
  | DisabledAnns of Ann.t list
    (* in addition to the default path map *)
  | PathMap of string HashArray.hash
    (* called only with the root source *)
  | PreprocessRoot of Basis.t -> Basis.t
    (* called for all other mlb files *)
  | Preprocess of Basis.t -> Basis.t
  | WarningHandler of warn -> unit

  type opts = opt list

  datatype err =
    Parse of Parse.err
  | Validation of Basis.err
  | Dag of Dag.err
  | Compilation of Compile.err
  | Exn of exn

  datatype 'a result =
    Ok of 'a
  | Error of err

  (* Default path map *)
  val pathMap : string HashArray.hash

  val compile : opts -> string -> NameSpace.t result

  (* Compile and import the content of the basis in the global namespace. *)
  val import : opts -> string -> unit result

  (* Compile and import an mlb file, much like the top level `use`. *)
  val use : string -> unit

  (* Format paths relative to the given one / absolute / relative to cwd.*)
  val warnToString  : string option -> warn -> string
  val warnToStringa : warn -> string
  val warnToStringd : warn -> string

  val errToString  : string option -> err -> string
  val errToStringa : err -> string
  val errToStringd : err -> string
end =
struct
  structure H   = HashArray
  structure OSF = OS.FileSys
  structure OSP = OS.Path

  datatype warn =
    BadAnn of string * PolyML.location
  | NoBasisInSMLLIB of string * string
  | RelativeSMLLIB of string * string

  datatype opt =
    AnnDefaults of Ann.t list
  | CompileOpts of Compile.opts
  | DisabledAnns of Ann.t list
  | PathMap of string HashArray.hash
  | PreprocessRoot of Basis.t -> Basis.t
  | Preprocess of Basis.t -> Basis.t
  | WarningHandler of warn -> unit

  type opts = opt list

  datatype err =
    Parse of Parse.err
  | Validation of Basis.err
  | Dag of Dag.err
  | Compilation of Compile.err
  | Exn of exn (* likely SysErr or IO.Io *)

  datatype 'a result =
    Ok of 'a
  | Error of err

  (* http://mlton.org/MLBasisPathMap
   * https://github.com/MLton/mlton/blob/master/mlton/control/control-flags.sml#L1636
   * - int, word and real can be deduced from precision / wordSize / radix
   * - target_arch from PolyML.architecture ()
   * - target_os from ?
   *   there is LibrarySupport.getOSType but it is not available
   *   val getOSCall: unit -> int = RunCall.rtsCallFast0 "PolyGetOSType"
   *   val getOS: int = getOSCall() -> 0 for Posix, 1 -> Windows
   *)
  val pathMap : string H.hash = H.hash 10

  local
    val cat = String.concat
    val int = Int.toString

    fun f2s _ "" = "-:"
      | f2s NONE f = f ^ ":"
      | f2s (SOME p) f = OSP.mkRelative { path = f, relativeTo = p } ^ ":"

    fun loc2s opt { file, startLine, startPosition, endLine, endPosition } =
      cat
        ( f2s opt file
        :: (if startLine = 0 then
            []
          else
            [ int startLine, ".", int startPosition, "-"
            , int endLine, ".", int endPosition, ": "
            ])
        )
  in
    fun warnToString opt =
      let
        val loc2s = loc2s opt
      in
        fn BadAnn (a, loc) =>
            cat [loc2s loc, "warning: unrecognized annotation '", a, "'"]
        | NoBasisInSMLLIB (s, p) =>
            cat
              [ "warning: basis library is not available at '", p
              , "' (SML_LIB = '", s, "')"
              ]
        | RelativeSMLLIB (s, r) =>
            cat [ "warning: SML_LIB is relative: '", r, "' (was '", s, "')"]
      end

    fun warnToStringa w = warnToString NONE w
    fun warnToStringd w = warnToString (SOME (OS.FileSys.getDir ())) w

    fun errToString opt =
      let
        val loc2s = loc2s opt
      in
        fn Parse { expected, found, at } =>
             cat
               [ loc2s at, "error: invalid grammer\nexpected "
               , case expected of
                   Parse.Dec     => "declaration"
                 | Parse.Exp     => "expression"
                 | Parse.LongId  => "qualified identifier"
                 | Parse.ShortId => "identifier"
                 | Parse.String  => "string constant"
               , " but found "
               , case found of
                   Parse.EOS       => "end of stream"
                 | Parse.Invalid t => "invalid token: '" ^ t ^ "'"
               ]
        | Validation (kind, s, at) =>
            cat
              [ loc2s at, "error: invalid declaration\n"
              , case kind of
                  Basis.DuplicateBind   => "rebound identifier"
                | Basis.Extension       => "invalid file extension"
                | Basis.UnboundVariable => "unbound path var"
              , ": '", s, "'"
              ]
        | Dag (Dag.Cycle l) =>
            cat
              (  "error: mlb cycle:\n"
              :: List.concat (List.map (fn s => ["\t", s, "\n"]) l)
              )
        | Compilation k =>
            cat
              [ case k of
                  Compile.Compilation (_, at) => loc2s at
                | Compile.Execution (f, _) => f2s opt f ^ " "
                | _ => ""
              , "error: aborted compilation:\n"
              , case k of
                  Compile.Compilation (s, _) => s
                | Compile.Dependency s => "Dependency invariant violated: " ^ s
                | Compile.Execution (_, e) =>
                    "raised during execution: " ^ exnMessage e
                | Compile.NoSuchMLB s => "no such mlb: " ^ s
                | Compile.UnboundId s => "unbound id: " ^ s
              ]
        | Exn e => "error: " ^ exnMessage e
      end

    fun errToStringa e = errToString NONE e
    fun errToStringd e = errToString (SOME (OS.FileSys.getDir ())) e
  end

  fun readFile p =
    let
      val s = TextIO.openIn p
    in
      TextIO.inputAll s before TextIO.closeIn s
    end

  local
    fun find f l v =
      let
        fun fd [] = NONE
          | fd (x::xs) = case f x of NONE => fd xs | z => z
      in
        Option.getOpt (fd l, v)
      end

    fun pp (z : Basis.t) = z
    fun wh (_ : warn) = ()
  in
    fun getCopts opts =
      find
        (fn CompileOpts c => SOME c | _ => NONE)
        opts { jobs = 1, depsFirst = false, copts = [] }

    fun doOpts opts =
      let
        fun fd f v = find f opts v
      in
        { anns     = fd (fn AnnDefaults l => SOME l | _ => NONE) []
        , dAnns    = fd (fn DisabledAnns l => SOME l | _ => NONE) []
        , pathMap  = fd (fn PathMap m => SOME m | _ => NONE) (H.hash 1)
        , preproc  = fd (fn Preprocess f => SOME f | _ => NONE) pp
        , rPreproc = fd (fn PreprocessRoot f => SOME f | _ => NONE) pp
        , warnCb   = fd (fn WarningHandler f => SOME f | _ => NONE) wh
        }
      end
  end

  fun doBasis f opts src =
    let
      val path = OSF.fullPath src
      val opts as { warnCb, ... } = doOpts opts

      val pathMap =
        let
          val h : string H.hash = H.hash 10
        in
          H.fold (fn (k, v, _) => H.update (h, k, v)) () pathMap;
          H.fold (fn (k, v, _) => H.update (h, k, v)) () (#pathMap opts);
          h
        end

      local
        open Path
        val smlLib = getOpt (H.sub (pathMap, "SML_LIB"), "")
      in
        val _ =
          case process pathMap "$(SML_LIB)/basis/basis.mlb" of
            Unbound _ => ()
          | Path p =>
              ( if (not o OSF.access) (p, [OSF.A_READ]) then
                  (warnCb o NoBasisInSMLLIB) (smlLib, p)
                else
                  ()
              ; if OSP.isRelative p then
                  (warnCb o RelativeSMLLIB)
                    (smlLib, String.substring (p, 0, String.size p - 16))
                else
                  ()
              )
      end

      fun convOpts p =
        { annCb = warnCb o BadAnn
        , disabledAnns = #dAnns opts
        , pathMap = pathMap
        , path = OSP.dir p
        , exts = NONE
        }

      fun mkBas p =
        ( #preproc opts
        o Basis.fromParse (convOpts p)
        o Parse.parse { fileName = p, lineOffset = 0 }
        o readFile
        ) p
    in
      Ok
        (( f
         o Dag.process mkBas
         o (fn b => (path, b))
         o #rPreproc opts
         o Basis.fromParse (convOpts path)
         o Parse.parse { fileName = path, lineOffset = 0 }
         ) src)
    end
      handle
        Parse.Parse z      => Error (Parse z)
      | Basis.Validation z => Error (Validation z)
      | Dag.Dag z          => Error (Dag z)
      | Compile.Compile z  => Error (Compilation z)
      | z                  => Error (Exn z)

  fun compile opts = doBasis (Compile.compile (getCopts opts)) opts

  fun import opts src =
    case compile opts src of
      Ok ns => Ok () before NameSpace.import (NameSpace.global, ns)
    | Error e => Error e

  local
    fun wh w = TextIO.print (warnToStringd w ^ "\n")
  in
    fun use s =
      case compile [WarningHandler wh] s of
        Ok ns => NameSpace.import (NameSpace.global, ns)
      | Error e =>
          (TextIO.print (errToStringd e ^ "\n"); raise Fail "Static errors")
  end
end


structure PolyMLB =
struct
  structure Ann = Ann
  structure Basis = Basis
  structure Compile = Compile
  structure Dag = Dag
  structure Lex = Lex
  structure NameSpace = NameSpace
  structure Parse = Parse
  structure Path = Path
  open PolyMLB
end
