signature DOCUMENT = sig
  datatype block_node = Paragraph of inline_node list
                      | List of list_item list
                      | Enumeration of list_item list
                      | DefList of definition list
                      | Image of string
                      | CodeBlock of string
                      | Quote of block_node list
                      | TexBlock of string
                      | Definition of string * block_node list
                      | Theorem of string * block_node list * block_node list
                      | Lemma of string * block_node list * block_node list
       and list_item = ListItem of block_node list
       and section = Section of string * inline_node list * block_node list * section list
       and inline_node = Whitespace
                       | Text of string
                       | Bold of inline_node list
                       | Italics of inline_node list
                       | Underline of inline_node list
                       | Superscript of inline_node list
                       | Subscript of inline_node list
                       | TeX of string
                       | Code of string
                       | WebLink of string * inline_node list
                       | InternalLink of string * inline_node list
                       | Foreign of inline_node list
                       | New of inline_node list
       and definition = Def of inline_node list * block_node list

  datatype author = Author of string * (string option) * (string option)

  datatype metadata = Metadata of string * author list

  datatype document = Document of metadata * section list

  (* Table of contents *)

  datatype toc = Toc of string * (inline_node list) * (toc list)

  val tableOfContents : document -> (toc list)
end
