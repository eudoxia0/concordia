signature CST = sig
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  val processIncludes : node -> node
end
