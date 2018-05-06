structure HtmlBackend : HTML_BACKEND = struct
  open Document
  open HtmlGen

  fun htmlInline (Whitespace) = String " "
    | htmlInline (Text s) = String s
    | htmlInline (Bold l) = n "strong" l
    | htmlInline (Italics l) = n "em" l
    | htmlInline (Underline l) = Node ("span", [Attr ("class", "underline")], wrap (map htmlInline l))
    | htmlInline (Superscript l) = n "sup" l
    | htmlInline (Subscript l) = n "sub" l
    | htmlInline (TeX s) = Node ("span", [Attr ("class", "inline-tex")], wrap [String ("$" ^ s ^ "$")])
    | htmlInline (Code s) = Node ("code", [], [String s])
    | htmlInline (InternalLink (id, l)) = Node ("a",
                                                [Attr ("href", "#" ^ id)],
                                                map htmlInline l)
    | htmlInline (Foreign l) = Node ("span", [Attr ("class", "foreign-text")], wrap (map htmlInline l))
    | htmlInline (New l) = Node ("span", [Attr ("class", "new-word")], wrap (map htmlInline l))
    | htmlInline _ = String "NOT IMPLEMENTED YET"
  and n name body = Node (name, [], wrap (map htmlInline body))
  and wrap l = l

  fun htmlBlock (Paragraph l) = Node ("p", [], map htmlInline l)
    | htmlBlock (List l) = Node ("ul", [], map listItem l)
    | htmlBlock (Enumeration l) = Node ("ol", [], map listItem l)
    | htmlBlock (DefList l) = Node ("dl", [], List.foldr (op @) [] (map defBody l))
    | htmlBlock (Image uri) = Node ("img", [Attr ("src", uri)], [])
    | htmlBlock (CodeBlock s) = Node ("pre", [], [Node ("code", [], [String s])])
    | htmlBlock (Quote l) = Node ("blockquote", [], map htmlBlock l)
    | htmlBlock (TexBlock s) = Node ("div", [Attr ("class", "block-tex")], [String ("\\(" ^ s ^ "\\)")])
    | htmlBlock (Definition (id, l)) = Node ("div", [Attr ("class", "admonition definition")],
                                             (admTitle "Definition: ") :: (map htmlBlock l))
    | htmlBlock (Theorem (id, s, p)) = metaTheorem "theorem" id s p
    | htmlBlock (Lemma (id, s, p)) = metaTheorem "lemma" id s p
  and listItem (ListItem l) = Node ("li", [], map htmlBlock l)
  and defBody (Def (t, d)) = [termBody t, defBody' d]
  and termBody l = Node ("dt", [], map htmlInline l)
  and defBody' l = Node ("dd", [], map htmlBlock l)
  and admTitle s = Node ("span", [cls "admonition-title"], [String s])
  and cls n = Attr ("class", n)
  and id' s = Attr ("id", s)
  and metaTheorem class id s p = let val s = map htmlBlock s
                                     and p = map htmlBlock p
                                 in
                                     let val sh = admTitle "Statement:"
                                         and ph = admTitle "Proof:"
                                     in
                                         Node ("div", [id' id, cls ("admonition " ^ class)], [
                                                   Node ("div", [cls "statement"], sh :: s),
                                                   Node ("div", [cls "proof"], ph :: p)
                                              ])
                                     end
                                 end

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

  fun htmlMeta (Metadata (title, authors)) cssFiles =
    let val title = Node ("title", [], [String title])
        and charset = (Node ("meta", [Attr ("charset", "UTF-8")], []))
        and css = map (fn s => String ("<link rel='stylesheet' href='" ^ s ^ "'>"))
                      cssFiles
    in
        title :: charset :: css
    end

  fun htmlHeader title = Node ("header", [], [
                                   Node ("h1",
                                         [Attr ("class", "title")],
                                         [String title])
                               ])

  fun htmlBody (Metadata (title, _)) secs toc =
    let val header = htmlHeader title
        val toc = Node ("ol", [Attr ("class", "toc")], map htmlToc toc)
        and sections = map (fn s => htmlSection s 1) secs
    in
        header :: toc :: sections
    end

  fun htmlDocument doc cssFiles jsFiles = htmlDocument' doc (tableOfContents doc) cssFiles jsFiles
  and htmlDocument' (Document (meta, secs)) toc cssFiles jsFiles =
    let val js = map (fn s => Node ("script", [Attr ("src", s)], [])) jsFiles
    in
        Node ("html", [], [
                  Node ("head", [], htmlMeta meta cssFiles),
                  Node ("body", [], (htmlBody meta secs toc) @ js)
             ])
    end
end
