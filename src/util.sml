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

structure Util : UTIL = struct
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    datatype 'a result = Result of 'a
                       | Failure of string

    fun isLeft (Left _) = true
      | isLeft _ = false

    fun isRight (Left _) = false
      | isRight _ = true

    fun readFileToString filepath =
      let val stream = TextIO.openIn filepath
          fun loop stream =
            case TextIO.inputLine stream of
                SOME line => line :: loop stream
              | NONE      => []
      in
          String.concat (loop stream before TextIO.closeIn stream)
      end

    fun writeStringToFile filepath str =
      let val stream = TextIO.openOut filepath
      in
          TextIO.output (stream, str) handle e => (TextIO.closeOut stream; raise e);
          TextIO.closeOut stream
      end

    fun readFileLines filepath =
      String.fields (fn c => c = #"\n") (readFileToString filepath)

    fun readFileRange filepath (s, e) =
      linesString (lineRange (readFileLines filepath) s e)
    and afterLine ls i = List.drop (ls, i - 1)
    and beforeLine ls i = List.take (ls, i - 1)
    and lineRange ls s e = beforeLine (afterLine ls s) (e - s + 2)
    and linesString ls = String.concatWith "\n" ls

    fun member (x, nil) = false
      | member (x, y::ys) = (x=y) orelse member (x, ys)

    fun afterPrefix str prefix =
      if (String.isPrefix prefix str) then
          SOME (String.extract (str, (String.size prefix), NONE))
      else
          NONE
end
