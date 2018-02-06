signature TRANSFORM = sig
  (*val parseBlock : CST.node -> Document.block_node

  val sectionNodes : Document.inline_node list -> Document.inline_node list
  val nonSectionNodes : Document.inline_node list -> Document.inline_node list
  val parseSection : CST.node -> Document.section*)

  val parseI : CST.node -> Document.inline_node
end

structure Transform = struct
  open Util;
  open Document;
  (* Utilities *)

  fun noArgument name = "'" ^ name ^ "' nodes don't take arguments."

  exception TransformFailure of string

  (* Transforming block nodes *)

  fun sectionNodes l = List.find isLeft l

  fun nonSectionNodes l = List.find isRight l

(*
  fun parseSectionContents l = map parseBlockOrSection l
  and parseBlockOrSection (CST.SList ("sec", SOME name, body)) = let val = parseSectionContents body
                                                                 in

                                                                 end
    | parseBlockOrSection (CST.SList ("sec", NONE , _))= raise TransformFailure "Section must have a name"
    | parseBlockOrSection (CST.SList (name, arg, body)) = Right (parseB (CST.SList (name, arg, body)))
    | parseBlockOrSection _ = raise TransformFailure "Text and TeX nodes are invalid section content"
  and parseB _ = Paragraph []
*)
  (*fun parseBlock _ = Paragraph []

  (* Transforming sections *)

  fun sectionNodes l = List.find (fn n => case n of
                                              (Section _) => true
                                            | _ => false)
                                 l

  fun nonSectionNodes l = List.find (fn n => case n of
                                              (Section _) => false
                                            | _ => true)
                                    l

  fun parseSection (CST.SList ("sec", SOME name, body)) = let val body' = map parseBlock body
                                                          in
                                                              let val content = nonSectionNodes body
                                                                  and subs = sectionNodes body
                                                              in
                                                                  raise TransformFailure "Not yet"
                                                              end
                                                          end
    | parseSection (CST.SList ("sec", NONE, body)) = raise TransformFailure "Sections must have names"
    | parseSection _ = raise TransformFailure "Tried to parse a non-section."*)

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
end
