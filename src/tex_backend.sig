signature TEX_BACKEND = sig
  type document_class = string option

  val texInline : Document.inline_node -> string
  val texBlock : Document.block_node -> string
  val texDocument : Document.document -> document_class -> string
end
