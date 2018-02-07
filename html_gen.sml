signature HTML_GEN = sig
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | Text of string
end

structure HtmlGen = struct
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | Text of string
end
