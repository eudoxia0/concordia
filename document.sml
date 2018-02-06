signature DOCUMENT = sig
  datatype block_node = Paragraph of inline_node
       and section = Section of block_node * section list
       and inline_node = Whitespace
                       | Text of string
                       | Bold of inline_node list
                       | Italics of inline_node list
                       | Underline of inline_node list
                       | Superscript of inline_node list
                       | Subscript of inline_node list
                       | Code of string
                       | WebLink of string * inline_node list
                       | DocumentLink of string * string * inline_node list
                       | Foreign of inline_node list
                       | New of inline_node list
end

structure Document : DOCUMENT = struct
  datatype block_node = Paragraph of inline_node
       and section = Section of block_node * section list
       and inline_node = Whitespace
                       | Text of string
                       | Bold of inline_node list
                       | Italics of inline_node list
                       | Underline of inline_node list
                       | Superscript of inline_node list
                       | Subscript of inline_node list
                       | Code of string
                       | WebLink of string * inline_node list
                       | DocumentLink of string * string * inline_node list
                       | Foreign of inline_node list
                       | New of inline_node list
end
