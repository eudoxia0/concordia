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

structure Parser : PARSER = struct
  open CST

  structure ps = Parsimony(ParsimonyStringInput)
  open ps

  (* Constants *)

  val startChar = #"\\"
  val leftDelimiter = #"{"
  val rightDelimiter = #"}"

  (* Utilities *)

  val whitespaceChars = [#" ", #"\n", #"\r"];

  (* Utility parsers *)

  val whitespaceParser = anyOf whitespaceChars;

  val ws = many whitespaceParser;

  (* Inline TeX *)

  val texDelimiter = #"$"

  val texChar = or (seqR (pchar #"\\") (pchar #"$")) (noneOf [texDelimiter]);

  val texParser = pmap String.implode (between (pchar texDelimiter) (many texChar) (pchar texDelimiter));

  (* Text *)

  val escapeBackslash = seqR (pchar #"\\") (pchar #"\\")

  val escapeLeftBracket = seqR (pchar #"\\") (pchar #"{")

  val escapeRightBracket = seqR (pchar #"\\") (pchar #"}")

  val textChar = or escapeBackslash
                    (or escapeLeftBracket
                        (or escapeRightBracket
                            (noneOf [startChar, leftDelimiter, rightDelimiter, #"$"])))

  val textParser = pmap String.implode (many1 textChar);

  (* Tags *)

  val tagChar = anyOfString "abcdefghijklmnopqrstuvwxyz_0123456789";

  val tagParser = seqR (pchar startChar)
                       (pmap String.implode (many1 tagChar))

  (* Tag argument *)

  val argumentChar = noneOf [#"]"]

  val argument = seqR (pchar #"[") (seqL (pmap String.implode (many1 argumentChar))
                                         (pchar #"]"))

  (* Structure *)

  fun defineNodeParser listParser =
    choice [pmap TeX texParser,
            pmap Text textParser,
            listParser];

  val listParser = (case wrapper () of
                        (nodeParser: node parser, nodeParserRef: node parser ref) =>
                        let val listParser = pmap (fn (tag, (arg, body)) => SList (tag, arg, body))
                                                  (seq tagParser (seq (opt argument)
                                                                      (between (pchar leftDelimiter)
                                                                               (many nodeParser)
                                                                               (pchar rightDelimiter))))

                        in
                            nodeParserRef := defineNodeParser listParser;
                            listParser
                        end);

  val sexpParser = defineNodeParser listParser;

  (* Functions *)

  fun parseString str =
    case (run sexpParser (ParsimonyStringInput.fromString str)) of
        (Success (node, _)) => Util.Result node
      | (Failure f) => Util.Failure (explain (Failure f))
end
