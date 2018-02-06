signature TRANSFORM = sig
  val parseI : CST.node -> Document.inline_node
end

structure Transform = struct
  open Util;
  open Document;
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

    | parseI (CST.SList ("f", NONE, body)) = Foreign (map parseI body)
    | parseI (CST.SList ("f", SOME _, _)) = raise TransformFailure (noArgument "Foreign")

    | parseI (CST.SList ("n", NONE, body)) = New (map parseI body)
    | parseI (CST.SList ("n", SOME _, _)) = raise TransformFailure (noArgument "New")

    | parseI _ = raise TransformFailure "Not implemented yet"

  (* Transforming block nodes *)

  fun extractTitle ((CST.SList ("title", SOME title, []))::body) = (title, body)
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
    let val (title, body) = extractTitle body
    in
        let val body' = parseSectionContents body
        in
            let val subsecs = sectionNodes body'
                and content = nonSectionNodes body'
            in
                Section (id, title, content, subsecs)
            end
        end
    end
  and parseSectionContents l = map parseBlockOrSection l
  and parseBlockOrSection (CST.SList ("sec", SOME name, body)) = Left (parseSection name body)
    | parseBlockOrSection (CST.SList ("sec", NONE , _))= raise TransformFailure "Section must have a name"
    | parseBlockOrSection (CST.SList (name, arg, body)) = Right (parseB (CST.SList (name, arg, body)))
    | parseBlockOrSection _ = raise TransformFailure "Text and TeX nodes are invalid section content"
  and parseB (CST.SList ("p", NONE, body)) = Paragraph (map parseI body)
    | parseB _ = Paragraph nil

  (* Parsing documents *)

  fun extractMetadata ((CST.SList ("metadata", NONE, meta))::body) = (meta, body)
    | extractMetadata _ = raise TransformFailure "Documents must start with a metadata node"

  fun parseDocument' body =
    let val (metadata, body') = extractMetadata body
    in
        let val meta = Metadata ("Untitled", [])
        in
            (* FIXME: ENSURE ALL BODY NODES ARE SECTION NODES *)
            let val children = map parseBlockOrSection body'
            in
                if (nonSectionNodes children) <> nil then
                    raise TransformFailure "Only sections are allowed as top-level nodes in a document after metadata"
                else
                    Document (meta, sectionNodes children)
            end
        end
    end
end
