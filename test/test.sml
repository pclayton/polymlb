let
  (* PolyML.make directly calls the top level print for its 'Making ...' and
   * 'Created ...' messages, so we manually redirect it to /dev/null. Simply
   * closing TextIO.stdOut or its underlying StreamIO stream causes unhandled
   * exceptions when print is called.
   *)
  open TextIO
  val stdout = getOutstream stdOut
  val devnull = openOut "/dev/null"
in
  (* use / make don't like absolute or ../ relative paths *)
  OS.FileSys.chDir "..";
  PolyML.suffixes := ".fun" :: !PolyML.suffixes;
  setOutstream (stdOut, getOutstream devnull);
  PolyML.make "src/lib/ThreadPools";
  use "src/lib/build.sml";
  closeOut devnull;
  setOutstream (stdOut, stdout);
  OS.FileSys.chDir "test"
end;

structure Test :>
sig
  type 'a t
  val assert : string * 'a -> 'a t
  val eq : ''a t * ''a -> unit
  val is : 'a t * ('a -> bool) -> unit
  val matches : 'a t * (('a * 'a -> bool) * 'a) -> unit
  val raises : (unit -> 'a) t * exn -> unit
  val raisesMatching : (unit -> 'a) t * (exn -> bool) -> unit
  val raisesExact : (unit -> 'a) t * exn -> unit
end =
struct
  type 'a t = string * 'a

  fun assert z = z

  fun fail s =
    ( TextIO.output (TextIO.stdErr, "Failed: " ^ s ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun eq ((s, a), e) = if a = e then () else fail s

  fun is ((s, a), f) = if f a then () else fail s

  fun matches ((s, a), (f, e)) = if f (e, a) then () else fail s

  fun raises ((s, f), e) =
    (f (); fail s)
    handle e' => if exnName e = exnName e' then () else fail s

  fun raisesMatching ((s, f), e) =
    (f (); fail s)
    handle e' => if e e' then () else fail s

  fun raisesExact ((s, f), e) =
    (f (); fail s)
    handle e' => if exnMessage e = exnMessage e' then () else fail s
end

infix assert eq is matches raises raisesMatching raisesExact
open Test
