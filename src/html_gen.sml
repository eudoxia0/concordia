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

structure HtmlGen : HTML_GEN = struct
  datatype attr = Attr of string * string

  datatype node = Node of string * attr list * node list
                | String of string

  (* FIXME: quote value string *)
  fun attrString (Attr (name, value)) = name ^ "=\"" ^ value ^ "\""

  fun generate' (Node (name, attrs, body)) = (headerString name attrs) ^
                                             (String.concat (map generate' body)) ^
                                             (footerString name)
    | generate' (String s) = (* FIXME: ESCAPE *) s
  and headerString name attrs = "<" ^ name ^ " " ^ (String.concatWith " " (map attrString attrs)) ^ ">"
  and footerString name = "</" ^ name ^ ">"

  fun generate node = "<!DOCTYPE html>\n" ^ (generate' node)
end
