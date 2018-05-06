structure TexBackend :> TEX_BACKEND = struct
  open Document

  fun escapeText s = String.translate (fn c => mapChar c) s
  and mapChar #"%" = "\\%"
    | mapChar #"&" = "\\&"
    | mapChar c = str c

  fun texInline Whitespace = ""
    | texInline (Text s) = escapeText s
    | texInline (Bold l) = "\\textbf{" ^ (concInline l) ^ "}"
    | texInline (Italics l) = "\\textit{" ^ (concInline l) ^ "}"
    | texInline (Underline l) = "\\underline{" ^ (concInline l) ^ "}"
    | texInline (Superscript l) = "\\textsuperscript" ^ (concInline l) ^ "}"
    | texInline (Subscript l) = "\\textsubscript" ^ (concInline l) ^ "}"
    | texInline (TeX s) = "$" ^ s ^ "$"
    | texInline (Code s) = "\\texttt{" ^ s ^ "}"
    | texInline (WebLink (url, l)) = "\\href{" ^ url ^ "}{" ^ (concInline l) ^ "}"
    | texInline (InternalLink (id, l)) = "\\hyperref[" ^ id ^ "]{" ^ (concInline l) ^ "}"
    | texInline (Foreign l) = "\\emph{" ^ (concInline l) ^ "}"
    | texInline (New l) = "\\textbf{" ^ (concInline l) ^ "}"
  and concInline l = String.concat (map texInline l)

  fun texBlock (Paragraph l) = (concInline l ^ "\n\n")
    | texBlock (List its) = "\\begin{itemize}\n"
                            ^ (String.concat (map (fn (ListItem l) => "\\item " ^ (concBlock l) ^ "\n\n")
                                                  its))
                            ^ "\\end{itemize}"
    | texBlock (Enumeration its) = "\\begin{enum}\n"
                                   ^ (String.concat (map (fn (ListItem l) => "\\item " ^ (concBlock l) ^ "\n\n")
                                                         its))
                                   ^ "\\end{enum}"
    | texBlock (DefList defs) = "\\begin{description}\n"
                                ^ (String.concat (map (fn (Def (t, d)) => "\\item [" ^ (concInline t) ^ "] " ^ (concBlock d) ^ "\n\n")
                                                      defs))
                                ^ "\\end{description}"
    | texBlock (Image path) = "\\includegraphics{" ^ path ^ "}"
    | texBlock (CodeBlock s) = s
    | texBlock (Quote l) = "\\begin{quotation}" ^ (concBlock l) ^ "\\end{quotation}"
    | texBlock (TexBlock s) = "\\[" ^ s ^ "\\]"
    | texBlock (Definition (id, l)) = concBlock l
    | texBlock (Theorem (id, s, p)) = concBlock s
    | texBlock (Lemma (id, s, p)) = concBlock s
  and concBlock l = String.concat (map texBlock l)

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

  fun texPrefix (Metadata (title, _)) =
    "\\documentclass{report}\n"
    ^ "\\usepackage[utf8]{inputenc}"
    ^ "\\usepackage{graphicx}"
    ^ "\n\n\\title{" ^ title ^ "}\n"
    ^ "\\date{\\today}\n\n"
    ^ "\\begin{document}\n"
    ^ "\\maketitle\n"
    ^ "\\tableofcontents\n\n"

  val texSuffix =
      "\\end{document}"

  fun texDocument (Document (meta, sections)) =
    (texPrefix meta) ^ (String.concat (map (fn s => texSection s 1) sections)) ^ "\n" ^ texSuffix
end