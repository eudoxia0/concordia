structure CST : CST = struct
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  fun processIncludes (SList ("include", SOME path, [])) = Text (Util.readFileToString path)
    | processIncludes (SList ("include", _, _)) = raise Fail "Bad \\include"
    | processIncludes (SList (n, a, l)) = SList (n, a, map processIncludes l)
    | processIncludes a = a
end
