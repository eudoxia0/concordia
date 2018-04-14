structure ToC : TOC = struct
  open Document

  datatype toc = Toc of (inline_node list) * (toc list)

  fun tableOfContents (Document (_, s)) = map toc s
  and toc (Section (id, title, _, secs)) = Toc (title, map toc secs)
end
