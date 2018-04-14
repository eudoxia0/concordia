structure HtmlGen : HTML_GEN = struct
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | String of string

  (* FIXME: quote value string *)
  fun attrString (Attr (name, value)) = name ^ "=\"" ^ value ^ "\""

  fun generate' (Node (name, attrs, body)) = (headerString name attrs) ^
                                             (String.concat (map generate' body)) ^
                                             (footerString name)
    | generate' (String s) = (* FIXME: ESCAPE *) s
  and headerString name attrs = "<" ^ name ^ " " ^ (String.concatWith " " (map attrString attrs)) ^ ">"
  and footerString name = "</" ^ name ^ ">"

  fun generate node = "<!DOCTYPE html>\n" ^ (generate' node)
end
