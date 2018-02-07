signature HTML_BACKEND = sig
  val htmlInline : Document.inline_node -> HtmlGen.node
end

structure HtmlBackend : HTML_BACKEND = struct
  open Document;
  open HtmlGen;

  fun htmlInline (Whitespace) = String " "
    | htmlInline (Text s) = String s
    | htmlInline (Bold l) = Node ("strong", [], map htmlInline l)
    | htmlInline (Italics l) = Node ("em", [], map htmlInline l)
    | htmlInline (Underline l) = Node ("span", [Attr ("class", "underline")], map htmlInline l)
    | htmlInline (Superscript l) = Node ("sup", [], map htmlInline l)
    | htmlInline (Subscript l) = Node ("sub", [], map htmlInline l)
    | htmlInline (TeX s) = Node ("span", [Attr ("class", "inline-tex")], [String s])
    | htmlInline _ = String "NOT IMPLEMENTED YET"
end
