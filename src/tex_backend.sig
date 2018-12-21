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

signature TEX_BACKEND = sig
  type document_class = string option
  type document_options = string option
  type main_font = string option

  val texInline : Document.inline_node -> string
  val texBlock : Document.block_node -> string
  val texDocument : Document.document -> document_class -> document_options -> main_font -> string
end
