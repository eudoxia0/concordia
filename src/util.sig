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

signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    datatype 'a result = Result of 'a
                       | Failure of string

    val isLeft : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool

    val readFileToString : string -> string
    val writeStringToFile : string -> string -> unit

    val readFileLines : string -> string list
    val readFileRange : string -> (int * int) -> string

    val member : (''a * ''a list) -> bool

    (* If the second string is a prefix of the first string, return the first
       string with the prefix removed *)
    val afterPrefix : string -> string -> string option
end
