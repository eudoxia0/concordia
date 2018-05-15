structure CST : CST = struct
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  fun includeFile path =
    Text (Util.readFileToString path)

  fun includeLines arg =
    let val tokens = String.tokens (fn c => c = #";") arg
    in
        case tokens of
            [path, s, e] => let val s' = forceInt s
                                and e' = forceInt e
                            in
                                Text (Util.readFileRange path (s', e'))
                            end
          | _ => raise Fail "Bad \\include"
    end
  and forceInt s =
      case (Int.fromString s) of
          SOME i => i
        | NONE => raise Fail ("Bad line number '" ^ s ^ "' for \\includelines")

  fun processIncludes (SList ("include", SOME path, [])) = includeFile path
    | processIncludes (SList ("include", _, _)) = raise Fail "Bad \\include"
    | processIncludes (SList ("includelines", SOME arg, [])) = includeLines arg
    | processIncludes (SList ("includelines", _, _)) = raise Fail "Bad \\includelines"
    | processIncludes (SList (n, a, l)) = SList (n, a, map processIncludes l)
    | processIncludes a = a
end
