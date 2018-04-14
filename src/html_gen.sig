signature HTML_GEN = sig
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | String of string

  val generate : node -> string
end
