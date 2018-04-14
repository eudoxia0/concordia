structure Transform = struct
  open Util
  open Document

  (* Utilities *)

  fun noArgument name = "'" ^ name ^ "' nodes don't take arguments."

  exception TransformFailure of string

  (* Transforming inline nodes *)

  fun parseI (CST.Text s) = Text s
    | parseI (CST.TeX s) = TeX s

    | parseI (CST.SList ("b", NONE, body)) = Bold (map parseI body)
    | parseI (CST.SList ("b", SOME _, _)) = raise TransformFailure (noArgument "Bold")

    | parseI (CST.SList ("i", NONE, body)) = Italics (map parseI body)
    | parseI (CST.SList ("i", SOME _, _)) = raise TransformFailure (noArgument "Italics")

    | parseI (CST.SList ("u", NONE, body)) = Underline (map parseI body)
    | parseI (CST.SList ("u", SOME _, _)) = raise TransformFailure (noArgument "Underline")

    | parseI (CST.SList ("sup", NONE, body)) = Superscript (map parseI body)
    | parseI (CST.SList ("sup", SOME _, _)) = raise TransformFailure (noArgument "Superscript")

    | parseI (CST.SList ("sub", NONE, body)) = Subscript (map parseI body)
    | parseI (CST.SList ("sub", SOME _, _)) = raise TransformFailure (noArgument "Subscript")

    | parseI (CST.SList ("c", NONE, body)) = Code ""
    | parseI (CST.SList ("c", SOME _, _)) = raise TransformFailure (noArgument "Code")

    | parseI (CST.SList ("ref", NONE, _)) = raise TransformFailure "Missing ID in ref"
    | parseI (CST.SList ("ref", SOME id, body)) = InternalLink (id, map parseI body)

    | parseI (CST.SList ("f", NONE, body)) = Foreign (map parseI body)
    | parseI (CST.SList ("f", SOME _, _)) = raise TransformFailure (noArgument "Foreign")

    | parseI (CST.SList ("n", NONE, body)) = New (map parseI body)
    | parseI (CST.SList ("n", SOME _, _)) = raise TransformFailure (noArgument "New")

    | parseI _ = raise TransformFailure "Not implemented yet"

  (* Transforming block nodes *)

  fun nonTextNodes l = List.mapPartial (fn x => case x of
                                                    (CST.Text s) => NONE
                                                  | a => SOME a)
                                       l

  fun extractTitle ((CST.SList ("title", NONE, title))::body) = (title, body)
    | extractTitle _ = raise TransformFailure "Section is missing title"

  fun sectionNodes (l: (section, block_node) either list)
    = List.mapPartial (fn x => case x of
                                   Left s => SOME s
                                 | Right _ => NONE)
                      l

  fun nonSectionNodes (l: (section, block_node) either list)
    = List.mapPartial (fn x => case x of
                                   Left _ => NONE
                                 | Right b => SOME b)
                      l

  fun parseSection id body =
    let val (title, body) = extractTitle (nonTextNodes body)
    in
        let val body' = parseSectionContents body
        in
            let val subsecs = sectionNodes body'
                and content = nonSectionNodes body'
            in
                Section (id, map parseI title, content, subsecs)
            end
        end
    end
  and parseSectionContents l = map parseBlockOrSection (nonTextNodes l)
  and parseBlockOrSection (CST.SList ("sec", SOME name, body)) = Left (parseSection name body)
    | parseBlockOrSection (CST.SList ("sec", NONE , _))= raise TransformFailure "Section must have a name"
    | parseBlockOrSection (CST.SList (name, arg, body)) = Right (parseB (CST.SList (name, arg, body)))
    | parseBlockOrSection _ = raise TransformFailure "Text and TeX nodes are invalid section content"
  and parseB (CST.SList ("p", NONE, body)) = Paragraph (map parseI body)

    | parseB (CST.SList ("li", NONE, l)) = List (map parseListItem (nonTextNodes l))

    | parseB (CST.SList ("ol", NONE, l)) = Enumeration (map parseListItem (nonTextNodes l))

    | parseB (CST.SList ("dl", NONE, l)) = DefList (parseDefListItems (nonTextNodes l))

    | parseB (CST.SList ("image", SOME uri, [])) = Image uri
    | parseB (CST.SList ("image", _, _)) = raise TransformFailure "Bad image definition"

    | parseB (CST.SList ("definition", SOME id, l)) = Definition (id, map parseB (nonTextNodes l))
    | parseB (CST.SList ("definition", NONE, l)) = raise TransformFailure "Definitions must have an ID"

    | parseB (CST.SList ("theorem", SOME id, l)) = let val (s, p) = parseTheorem l
                                                   in
                                                       Theorem (id, s, p)
                                                   end
    | parseB (CST.SList ("theorem", NONE, l)) = raise TransformFailure "Theorems must have an ID"

    | parseB (CST.SList ("lemma", SOME id, l)) = let val (s, p) = parseTheorem l
                                                 in
                                                     Lemma (id, s, p)
                                                 end
    | parseB (CST.SList ("lemma", NONE, l)) = raise TransformFailure "Lemmas must have an ID"

    | parseB (CST.SList (n, _, _)) = raise TransformFailure (n ^ ": Not implemented yet")
    | parseB _ = raise TransformFailure "Bad text or tex node"
  and parseListItem (CST.SList ("it", NONE, l)) = ListItem (map parseB (nonTextNodes l))
    | parseListItem _ = raise TransformFailure "Bad list item definition"
  and parseDefListItems ((CST.SList ("term", NONE, t))::(CST.SList ("def", NONE, d))::rest) =
      (Def (map parseI t, map parseB (nonTextNodes d))) :: (parseDefListItems rest)
    | parseDefListItems nil = nil
    | parseDefListItems _ = raise TransformFailure "Error when parsing definition list"
  and parseTheorem l = case (nonTextNodes l) of
                           [CST.SList ("statement", NONE, s),
                            CST.SList ("proof", NONE, p)] => (map parseB (nonTextNodes s),
                                                              map parseB (nonTextNodes p))
                         | _ => raise TransformFailure "Bad theorem or lemma"

  (* Parsing documents *)

  fun extractMetadata ((CST.SList ("metadata", NONE, meta))::body) = (meta, body)
    | extractMetadata _ = raise TransformFailure "Documents must start with a metadata node"

  fun parseMetadata title meta = Metadata (title, [])

  fun parseDocument node = (Util.Result (parseDocument' node))
                           handle (TransformFailure msg) => (Util.Failure msg)
  and parseDocument' node = case node of
                                (CST.SList ("document", SOME title, body)) => parseDocument'' title body
                              | (CST.SList ("document", NONE, body)) => raise TransformFailure "Missing document title"
                              | _ => raise TransformFailure "Invalid document form"
  and parseDocument'' title body =
    let val (metadata, body') = extractMetadata (nonTextNodes body)
    in
        let val meta = parseMetadata title metadata
            and children = map parseBlockOrSection body'
        in
            if (nonSectionNodes children) <> nil then
                raise TransformFailure "Only sections are allowed as top-level nodes in a document after metadata"
            else
                Document (meta, sectionNodes children)
        end
    end
end