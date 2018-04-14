signature TOC = sig
  datatype toc = Toc of (Document.inline_node list) * (toc list)

  val tableOfContents : Document.document -> (toc list)
end
