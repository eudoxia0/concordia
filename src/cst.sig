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

signature CST = sig
  datatype node = Text of string
                | TeX of string
                | SList of string * (string option) * node list

  val processIncludes : node -> node
end
