structure Path :
sig
  datatype t = Path of string | Unbound of string

  val process : string HashArray.hash -> string -> t
end =
struct
  structure S = String

  datatype t = Path of string | Unbound of string

  fun process m s =
    let
      val var = ref false
      val sz = S.size s

      fun find (i, c) =
        let
          fun f i =
            if i = sz then
              NONE
            else if CharVector.sub (s, i) = c then
              SOME i
            else
              f (i + 1)
        in
          f i
        end

      fun f (i, l) =
        if i >= sz then
          l
        else
          case find (i, #"$") of
            NONE => S.extract (s, i, NONE) :: l
          | SOME j =>
              if j > sz - 2 then
                S.extract (s, i, NONE) :: l
              else if CharVector.sub (s, j + 1) <> #"(" then
                f (j + 1, "$":: l)
              else
                case find (j + 2, #")") of
                  NONE => S.extract (s, i, NONE) :: l
                | SOME k =>
                    (case (var := true; S.substring (s, j + 2, k - j - 2)) of
                      (* todo: what's the correct behavior here?
                       * mlton checks for an empty var '' but does not allow
                       * setting it through -mlb-path-{map,var}
                       *)
                        (* "" => f (k + 1, "$()"::l) *)
                        "" => raise Fail ""
                      | s' =>
                          (case HashArray.sub (m, s') of
                            NONE =>
                              raise Fail s'
                          | SOME v =>
                              f (k + 1, v :: S.substring (s, i, j - i) :: l)))

      val s' = (S.concat o List.rev o f) (0, [])
    in
      if !var then
        process m s'
      else
        Path s'
    end
      handle Fail v => Unbound v
end
