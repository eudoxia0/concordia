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

structure TexBackend :> TEX_BACKEND = struct
    open Document

    type document_class = string option
    type document_options = string option
    type main_font = string option

    fun escapeText s = String.translate (fn c => mapChar c) s
    and mapChar #"%" = "\\%"
      | mapChar #"&" = "\\&"
      | mapChar #"^" = "\\^"
      | mapChar #"#" = "\\#"
      | mapChar c = str c

    fun texInline Whitespace =
        ""
      | texInline (Text s) =
        escapeText s
      | texInline (Bold l) =
        "\\textbf{" ^ (concInline l) ^ "}"
      | texInline (Italics l) =
        "\\textit{" ^ (concInline l) ^ "}"
      | texInline (Underline l) =
        "\\underline{" ^ (concInline l) ^ "}"
      | texInline (Superscript l) =
        "\\textsuperscript" ^ (concInline l) ^ "}"
      | texInline (Subscript l) =
        "\\textsubscript" ^ (concInline l) ^ "}"
      | texInline (TeX s) =
        "$" ^ s ^ "$"
      | texInline (Code l) =
        "\\texttt{" ^ (concInline l) ^ "}"
      | texInline (WebLink (url, l)) =
        "\\href{" ^ url ^ "}{" ^ (concInline l) ^ "}"
      | texInline (InternalLink (id, l)) =
        "\\hyperref[" ^ id ^ "]{" ^ (concInline l) ^ "}"
      | texInline (Foreign l) =
        "\\emph{" ^ (concInline l) ^ "}"
      | texInline (New l) =
        "\\textbf{" ^ (concInline l) ^ "}"
    and concInline l = String.concat (map texInline l)

    fun texBlock (Paragraph l) =
        (concInline l ^ "\n\n")
      | texBlock (List its) =
        "\\begin{itemize}\n"
        ^ (String.concat (map (fn (ListItem l) => "\\item " ^ (concBlock l) ^ "\n\n")
                              its))
        ^ "\\end{itemize}\n\n"
      | texBlock (Enumeration its) =
        "\\begin{enum}\n"
        ^ (String.concat (map (fn (ListItem l) => "\\item " ^ (concBlock l) ^ "\n\n")
                              its))
        ^ "\\end{enum}\n\n"
      | texBlock (DefList defs) =
        "\\begin{description}\n"
        ^ (String.concat (map (fn (Def (t, d)) => "\\item [" ^ (concInline t) ^ "] " ^ (concBlock d) ^ "\n\n")
                              defs))
        ^ "\\end{description}\n\n"
      | texBlock (Image path) =
        "\\includegraphics{" ^ path ^ "}\n\n"
      | texBlock (CodeBlock s) =
        "\\begin{verbatim}" ^ s ^ "\\end{verbatim}\n\n"
      | texBlock (Quote l) =
        "\\begin{quotation}" ^ (concBlock l) ^ "\\end{quotation}\n\n"
      | texBlock (TexBlock s) =
        "\\[" ^ s ^ "\\]"
      | texBlock (Table { title, header, body, footer }) =
        renderTable title header body footer
      | texBlock (Definition (id, l)) =
        concBlock l
      | texBlock (Theorem (id, s, p)) =
        concBlock s
      | texBlock (Lemma (id, s, p)) =
        concBlock s

    and concBlock l = String.concat (map texBlock l)

    and renderTable title header body footer =
        let fun renderRows l =
                String.concat (map renderRow l)
        in
            let val title' =
                    case title of
                        SOME title => "\\caption{"
                                      ^ concInline title
                                      ^ "}"
                      | NONE => ""

                and header' =
                    case header of
                        SOME l => ((renderRows l) ^ "\\hline")
                      | NONE => ""

                and body' =
                    renderRows body

                and footer' =
                    case header of
                        SOME l => ("\\hline" ^ (renderRows l))
                      | NONE => ""
            in
                "\\begin{tabular}\n"
                ^ title' ^ "\n"
                ^ header' ^ "\n"
                ^ body' ^ "\n"
                ^ footer' ^ "\n"
                ^ "\\end{tabular}"
            end
        end

    and renderRow (TableRow cells) =
        (String.concatWith " & " (map renderCell cells)) ^ "\\\\\n"

    and renderCell (TableCell l) =
        concBlock l

    fun sectionTag 1 = "\\part"
      | sectionTag 2 = "\\chapter"
      | sectionTag 3 = "\\section"
      | sectionTag 4 = "\\subsection"
      | sectionTag 5 = "\\subsubsection"
      | sectionTag _ = "\\paragraph"

    fun texSection (Section (id, title, contents, subsecs)) depth =
        "\n\n" ^ (sectionTag depth) ^ "{" ^ (concInline title) ^ "}\n"
        ^ "\\label{" ^ id ^ "}\n"
        ^ (concBlock contents)
        ^ (String.concat (map (fn s => texSection s (depth+1)) subsecs))

    fun texDocOptions (SOME s) = "[" ^ s ^ "]"
      | texDocOptions NONE = ""

    fun texPrefix (Metadata (title, _)) docclass docoptions font =
        "\\documentclass" ^ (texDocOptions docoptions) ^ "{" ^ docclass ^ "}\n"
        ^ "\\usepackage[utf8]{inputenc}\n"
        ^ "\\usepackage{graphicx}\n"
        ^ "\\usepackage{listings}\n"
        ^ "\\usepackage{fontspec}\n"
        ^ (case font of
               SOME f => "\\setmainfont{" ^ f ^ "}\n"
             | NONE => "")
        ^ "\n\n\\title{" ^ title ^ "}\n"
        ^ "\\date{\\today}\n\n"
        ^ "\\begin{document}\n"
        ^ "\\maketitle\n"
        ^ "\\tableofcontents\n\n"

    val texSuffix =
        "\\end{document}"

    fun texDocument (Document (meta, sections)) docclass docoptions mainFont =
        let val docclass' = Option.getOpt (docclass, "report")
        in
            (texPrefix meta docclass' docoptions mainFont)
            ^ (String.concat (map (fn s => texSection s 1) sections))
            ^ "\n"
            ^ texSuffix
        end
end
