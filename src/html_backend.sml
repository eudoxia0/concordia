(*
    This file is part of Concordia.

    Concordia is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Concordia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Concordia.  If not, see <https://www.gnu.org/licenses/>.
*)

structure HtmlBackend : HTML_BACKEND = struct
    open Document
    open HtmlGen

    fun escapeHtml s =
        (* TODO: implement this lel *)
        s

    fun htmlInline (Whitespace) =
        String " "
      | htmlInline (Text s) =
        String (escapeHtml s)
      | htmlInline (Bold l) =
        n "strong" l
      | htmlInline (Italics l) =
        n "em" l
      | htmlInline (Underline l) =
        Node ("span",
              [Attr ("class", "underline")],
              map htmlInline l)
      | htmlInline (Superscript l) =
        n "sup" l
      | htmlInline (Subscript l) =
        n "sub" l
      | htmlInline (TeX s) =
        Node ("span",
              [Attr ("class", "inline-tex")],
              [String ("$" ^ s ^ "$")])
      | htmlInline (Code l) =
        n "code" l
      | htmlInline (WebLink (uri, l)) =
        Node ("a",
              [Attr ("href", uri)],
              map htmlInline l)
      | htmlInline (InternalLink (id, l)) =
        Node ("a",
              [Attr ("href", "#" ^ id)],
              map htmlInline l)
      | htmlInline (Foreign l) =
        Node ("span", [Attr ("class", "foreign-text")], map htmlInline l)
      | htmlInline (New l) =
        Node ("span", [Attr ("class", "new-word")], map htmlInline l)

    and n name body = Node (name, [], map htmlInline body)

    fun htmlBlock (Paragraph l) =
        Node ("p", [], map htmlInline l)
      | htmlBlock (List l) =
        Node ("ul", [], map listItem l)
      | htmlBlock (Enumeration l) =
        Node ("ol", [], map listItem l)
      | htmlBlock (DefList l) =
        Node ("dl", [], List.foldr (op @) [] (map defBody l))
      | htmlBlock (Image uri) =
        Node ("img", [Attr ("src", uri)], [])
      | htmlBlock (CodeBlock s) =
        Node ("pre", [], [
                  Node ("code", [], [String s])])
      | htmlBlock (Quote l) =
        Node ("blockquote", [], map htmlBlock l)
      | htmlBlock (TexBlock s) =
        Node ("div", [Attr ("class", "block-tex")], [String ("\\(" ^ s ^ "\\)")])
      | htmlBlock (Table { title, header, body, footer }) =
        renderTable title header body footer
      | htmlBlock (Definition (id, l)) =
        Node ("div", [Attr ("class", "admonition definition")],
              (admTitle "Definition: ") :: (map htmlBlock l))
      | htmlBlock (Theorem (id, s, p)) =
        metaTheorem "theorem" id s p
      | htmlBlock (Lemma (id, s, p)) =
        metaTheorem "lemma" id s p

    and listItem (ListItem l) = Node ("li", [], map htmlBlock l)

    and defBody (Def (t, d)) = [termBody t, defBody' d]

    and termBody l = Node ("dt", [], map htmlInline l)

    and defBody' l = Node ("dd", [], map htmlBlock l)

    and renderTable title header body footer =
        let val title' =
                case title of
                    SOME l => SOME (Node ("caption", [], map htmlInline l))
                  | NONE => NONE

            and header' =
                case header of
                    SOME l => SOME (Node ("thead", [], map renderRow l))
                  | NONE => NONE

            and body' =
                SOME (Node ("tbody", [], map renderRow body))

            and footer' =
                case footer of
                    SOME l => SOME (Node ("tfoot", [], map renderRow l))
                  | NONE => NONE
        in
            let val nodes = List.mapPartial (fn x => x) [title', header', body', footer']
            in
                Node ("table", [], nodes)
            end
        end

    and renderRow (TableRow cells) =
        Node ("tr", [], map renderCell cells)

    and renderCell (TableCell l) =
        Node ("td", [], map htmlBlock l)

    and admTitle s = Node ("span", [cls "admonition-title"], [String s])

    and cls n = Attr ("class", n)

    and id' s = Attr ("id", s)

    and metaTheorem class id s p =
        let val s = map htmlBlock s
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

    fun heading depth =
        if depth < 7 then
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

    fun htmlHeader title =
        Node ("header", [], [
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

    fun htmlDocument doc cssFiles jsFiles macros =
        htmlDocument' doc
                      (tableOfContents doc)
                      cssFiles
                      jsFiles
                      macros

    and htmlDocument' (Document (meta, secs)) toc cssFiles jsFiles macros =
        let val js = map (fn s => Node ("script", [Attr ("src", s)], [])) jsFiles
            and mathjax = Node ("script", [], [String (TexMacros.mathJaxConfig macros)])
        in
            Node ("html", [], [
                      Node ("head", [], htmlMeta meta cssFiles),
                      Node ("body", [], (htmlBody meta secs toc) @ js @ [mathjax])
                 ])
        end
end
