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

  val texChar = or (seqR (pchar #"\\") (pchar #"$")) (noneOf [#"$"]);

  val texParser = pmap String.implode (between (pchar #"$") (many texChar) (pchar #"$"));

  (* Text *)

  val textChar = noneOf [startChar, rightDelimiter, #"$"];

  val textParser = pmap String.implode (many1 textChar);

  (* Tags *)

  val tagChar = anyOfString "abcdefghijklmnopqrstuvwxyz-0123456789";

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
