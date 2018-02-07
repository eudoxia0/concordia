signature HTML_BACKEND = sig
  val htmlInline : Document.inline_node -> HtmlGen.node
end

structure HtmlBackend : HTML_BACKEND = struct
  open Document;
  open HtmlGen;

  fun htmlInline (Whitespace) = String " "
    | htmlInline (Text s) = String s
    | htmlInline _ = String ""
end
