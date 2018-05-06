signature TEX_BACKEND = sig
  val texInline : Document.inline_node -> string
  val texBlock : Document.block_node -> string
  val texDocument : Document.document -> string
end
