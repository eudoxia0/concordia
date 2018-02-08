signature HTML_BACKEND = sig
  val htmlInline : Document.inline_node -> HtmlGen.node
  val htmlBlock : Document.block_node -> HtmlGen.node
  val htmlDocument : Document.document -> HtmlGen.node
end

structure HtmlBackend : HTML_BACKEND = struct
  open Document;
  open HtmlGen;

  fun htmlInline (Whitespace) = String " "
    | htmlInline (Text s) = String s
    | htmlInline (Bold l) = n "strong" l
    | htmlInline (Italics l) = n "em" l
    | htmlInline (Underline l) = Node ("span", [Attr ("class", "underline")], wrap (map htmlInline l))
    | htmlInline (Superscript l) = n "sup" l
    | htmlInline (Subscript l) = n "sub" l
    | htmlInline (TeX s) = Node ("span", [Attr ("class", "inline-tex")], wrap [String s])
    | htmlInline (Code s) = Node ("code", [], [String s])
    | htmlInline (Foreign l) = Node ("span", [Attr ("class", "foreign-text")], wrap (map htmlInline l))
    | htmlInline (New l) = Node ("span", [Attr ("class", "new-word")], wrap (map htmlInline l))
    | htmlInline _ = String "NOT IMPLEMENTED YET"
  and n name body = Node (name, [], wrap (map htmlInline body))
  and wrap l = (String " ") :: l @ [String " "]

  fun htmlBlock (Paragraph l) = Node ("p", [], map htmlInline l)

  fun heading depth = if depth < 7 then
                          ("h" ^ (Int.toString depth))
                      else
                          "h6"

  fun htmlSection (Section (id, title, contents, subsecs)) depth =
    Node ("section", [Attr ("id", id)],
          let val title = (Node (heading depth, [], map htmlInline title))
              and content = map htmlBlock contents
              and subsecs = map (fn s => htmlSection s (depth + 1)) subsecs
          in
              title :: content @ subsecs
          end)

  val jsCode = "var nodes = document.getElementsByClassName('inline-tex');" ^
               "for (var i = 0; i < nodes.length; i++) {" ^
               "    var node = nodes[i];" ^
               "    var text = node.innerHTML;" ^
               "    katex.render(text, nodes[i]);" ^
               "}"

  fun htmlMeta meta = [
      Node ("meta", [Attr ("charset", "UTF-8")], []),
      Node ("script", [Attr ("src", "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0-beta1/katex.min.js")], []),
      String "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0-beta1/katex.min.css'>",
      Node ("style", [], [String ".inline-tex { padding: 0 5px; }"])
  ]

  fun htmlDocument (Document (meta, secs)) =
    Node ("html", [], [
              Node ("head", [], htmlMeta meta),
              Node ("body", [], (map (fn s => htmlSection s 1) secs) @
                                [Node ("script", [], [String jsCode])])
          ])
end
