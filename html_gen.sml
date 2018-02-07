signature HTML_GEN = sig
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | Text of string

  val generate : node -> string
end

structure HtmlGen = struct
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | Text of string

  (* FIXME: quote value string *)
  fun attrString (Attr (name, value)) = name ^ "=\"" ^ value ^ "\""

  fun generate (Node (name, attrs, body)) = (headerString name attrs) ^
                                            (String.concat (map generate body)) ^
                                            (footerString name)
  and headerString name attrs = "<" ^ name ^ " " ^ (String.concatWith " " (map attrString attrs)) ^ ">"
  and footerString name = "</" ^ name ^ ">"
end
