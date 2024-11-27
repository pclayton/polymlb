structure PolyMLB :
sig
  datatype opt =
    AnnDefaults of Ann.t list
  | CompileOpts of Compile.opts
    (* dummy values can be used; the anns will be completely disabled *)
  | DisabledAnns of Ann.t list
  | Logger of Log.logger
    (* in addition to the default path map *)
  | PathMap of string HashArray.hash
    (* called only with the root source *)
  | PreprocessRoot of Basis.t -> Basis.t
    (* called for all other mlb files *)
  | Preprocess of Basis.t -> Basis.t

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

  (* path fmt *)
  val errToString  : (string -> string) -> err -> string
end =
struct
  structure H   = HashArray
  structure OSF = OS.FileSys
  structure OSP = OS.Path

  datatype opt =
    AnnDefaults of Ann.t list
  | CompileOpts of Compile.opts
  | DisabledAnns of Ann.t list
  | Logger of Log.logger
  | PathMap of string HashArray.hash
  | PreprocessRoot of Basis.t -> Basis.t
  | Preprocess of Basis.t -> Basis.t

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
  in
    fun errToString fmt =
      let
        val loc2s = Log.locFmt fmt
      in
        fn Parse { expected, found, at } =>
             cat
               [ loc2s at, ": error: invalid grammar\nexpected "
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
              [ loc2s at, ": error: invalid declaration\n"
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
                | Compile.Execution (f, _) => fmt f ^ " "
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
    val log = { pathFmt = fn z => z, print = fn _ => () }
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
        , logger   = fd (fn Logger ff => SOME ff | _ => NONE) log
        , pathMap  = fd (fn PathMap m => SOME m | _ => NONE) (H.hash 1)
        , preproc  = fd (fn Preprocess f => SOME f | _ => NONE) pp
        , rpreproc = fd (fn PreprocessRoot f => SOME f | _ => NONE) pp
        }
      end
  end

  fun doBasis f opts src =
    let
      val opts as { logger, ... } = doOpts opts

      val pathMap =
        let
          val h : string H.hash = H.hash 10
        in
          H.fold (fn (k, v, _) => H.update (h, k, v)) () pathMap;
          H.fold (fn (k, v, _) => H.update (h, k, v)) () (#pathMap opts);
          h
        end

      fun convOpts p =
        { disabledAnns = #dAnns opts
        , pathMap = pathMap
        , path = OSP.dir p
        , exts = NONE
        }

      fun mkBas p =
        ( (if p = src then #rpreproc else #preproc) opts
        o Basis.fromParse logger (convOpts p)
        o Parse.parse { fileName = p, lineOffset = 0 }
        o readFile
        ) p
    in
      (Ok o f logger o Dag.process logger mkBas o OSF.fullPath) src
      handle x =>
        let
          val { pathFmt, print } = logger
          val err =
            case x of
              Parse.Parse z      => Parse z
            | Basis.Validation z => Validation z
            | Dag.Dag z          => Dag z
            | Compile.Compile z  => Compilation z
            | z                  => Exn z
        in
          print (Log.Error, errToString pathFmt err);
          Error err
        end
    end

  fun compile opts = doBasis (fn f => Compile.compile f (getCopts opts)) opts

  fun import opts src =
    case compile opts src of
      Ok ns => Ok () before NameSpace.import (NameSpace.global, ns)
    | Error e => Error e

  local
    fun fmt p = OSP.mkRelative { path = p, relativeTo = (OSF.getDir ()) }
    fun log (Log.Warn, m) = print ("warning: " ^ m ^ "\n")
      | log (Log.Error, m) = print ("error" ^ m ^ "\n")
      | log _ = ()
  in
    fun use s =
      case compile [Logger { pathFmt = fmt, print = log }] s of
        Ok ns => NameSpace.import (NameSpace.global, ns)
      | Error _ => raise Fail "Static errors"
  end
end


structure PolyMLB =
struct
  structure Ann = Ann
  structure Basis = Basis
  structure Compile = Compile
  structure Dag = Dag
  structure Lex = Lex
  structure Log = Log
  structure NameSpace = NameSpace
  structure Parse = Parse
  structure Path = Path
  open PolyMLB
end
