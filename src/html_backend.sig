signature HTML_BACKEND = sig
  val htmlInline : Document.inline_node -> HtmlGen.node
  val htmlBlock : Document.block_node -> HtmlGen.node
  val htmlDocument : Document.document -> string list -> string list -> HtmlGen.node
end
