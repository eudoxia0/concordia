signature CST = sig
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  datatype result = Result of node
                  | Fail of string
end

structure CST : CST = struct
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  datatype result = Result of node
                  | Fail of string
end
