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

signature DOCUMENT = sig
    datatype block_node = Paragraph of inline_node list
                        | List of list_item list
                        | Enumeration of list_item list
                        | DefList of definition list
                        | Image of string
                        | CodeBlock of string
                        | Quote of block_node list
                        | TexBlock of string
                        | Table of {
                            title : (inline_node list) option,
                            header : (row list) option,
                            body : row list,
                            footer : (row list) option
                        }
                        | Definition of string * block_node list
                        | Theorem of string * block_node list * block_node list
                        | Lemma of string * block_node list * block_node list

         and list_item = ListItem of block_node list

         and row = TableRow of cell list

         and cell = TableCell of block_node list

         and section = Section of string * inline_node list * block_node list * section list

         and inline_node = Whitespace
                         | Text of string
                         | Bold of inline_node list
                         | Italics of inline_node list
                         | Underline of inline_node list
                         | Superscript of inline_node list
                         | Subscript of inline_node list
                         | TeX of string
                         | Code of inline_node list
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
