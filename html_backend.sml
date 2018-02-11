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
  and wrap l = l

  fun htmlBlock (Paragraph l) = Node ("p", [], map htmlInline l)
    | htmlBlock (List l) = Node ("ul", [], map listItem l)
    | htmlBlock (Enumeration l) = Node ("ol", [], map listItem l)
    | htmlBlock (Image uri) = Node ("img", [Attr ("src", uri)], [])
    | htmlBlock (Definition (id, l)) = Node ("div", [Attr ("class", "definition")],
                                             (String "Definition: ") :: (map htmlBlock l))
  and listItem (ListItem l) = Node ("li", [], map htmlBlock l)

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

  (* Table of contents *)

  fun htmlToc (Toc (id, title, tocs)) =
    Node ("li", [], [
              Node ("a", [Attr ("href", "#" ^ id)], (map htmlInline title)),
              Node ("ol", [], map htmlToc tocs)
         ])

  (* Template *)

  val jsCode = "var nodes = document.getElementsByClassName('inline-tex');" ^
               "for (var i = 0; i < nodes.length; i++) {" ^
               "    var node = nodes[i];" ^
               "    var text = node.textContent;" ^
               "    katex.render(text, nodes[i]);" ^
               "}"

  fun htmlMeta meta = [
      Node ("meta", [Attr ("charset", "UTF-8")], []),
      Node ("script", [Attr ("src", "assets/katex/katex.min.js")], []),
      String "<link rel='stylesheet' href='assets/katex/katex.min.css'>",
      String "<link rel='stylesheet' href='assets/style.css'>"
  ]

  fun htmlDocument doc = htmlDocument' (tableOfContents doc) doc
  and htmlDocument' toc (Document (meta, secs)) =
    let val toc = Node ("ol", [], map htmlToc toc)
        and sections = map (fn s => htmlSection s 1) secs
        and js = [Node ("script", [Attr ("class", "toc")], [String jsCode])]
    in
        Node ("html", [], [
                  Node ("head", [], htmlMeta meta),
                  Node ("body", [], toc :: sections @ js)
             ])
    end
end
