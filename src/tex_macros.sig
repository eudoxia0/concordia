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

signature TEX_MACROS = sig
  type macro

  val macroName : macro -> string
  val macroArity : macro -> int
  val macroDefinition : macro -> string

  val toString : macro -> string

  val mathJaxConfig : macro list -> string

  val parseMacroFile : string -> macro list
end
