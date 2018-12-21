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

structure ToC : TOC = struct
  open Document

  datatype toc = Toc of (inline_node list) * (toc list)

  fun tableOfContents (Document (_, s)) = map toc s
  and toc (Section (id, title, _, secs)) = Toc (title, map toc secs)
end
