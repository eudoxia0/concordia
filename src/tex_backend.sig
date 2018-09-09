signature TEX_BACKEND = sig
  type document_class = string option
  type document_options = string option
  type main_font = string option

  val texInline : Document.inline_node -> string
  val texBlock : Document.block_node -> string
  val texDocument : Document.document -> document_class -> document_options -> main_font -> string
end
