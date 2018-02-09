signature PARSER = sig
  val parseString : string -> CST.result
end

structure Parser : PARSER = struct
  open ParCom
  open CST

  (* Constants *)

  val startChar = #"\\"
  val leftDelimiter = #"{"
  val rightDelimiter = #"}"

  (* Utilities *)

  val whitespaceChars = [#" ", #"\n", #"\r"];

  fun cleanUp str = str

  (* Utility parsers *)

  val whitespaceParser = anyOf whitespaceChars;

  val ws = many whitespaceParser;

  (* Inline TeX *)

  val texChar = orElse (andThenR (pchar #"\\") (pchar #"$")) (npchar #"$");

  val texParser = pmap String.implode (between (pchar #"$") (many texChar) (pchar #"$"));

  (* Text *)

  val textChar = noneOf [startChar, rightDelimiter, #"$"];

  val textParser = pmap String.implode (many1 textChar);

  (* Tags *)

  val tagChar = anyOfString "abcdefghijklmnoprstuvwxyz-0123456789";

  val tagParser = andThenR (pchar startChar)
                           (pmap String.implode (many1 tagChar))

  (* Tag argument *)

  val argumentChar = noneOf [#"]"]

  val argument = andThenR (pchar #"[") (andThenL (pmap String.implode (many1 argumentChar))
                                                 (pchar #"]"))

  (* Structure *)

  fun defineNodeParser listParser =
    choice [pmap TeX texParser,
            pmap (Text o cleanUp) textParser,
            listParser];

  val listParser = (case createWrapperParser () of
                        (nodeParser: node parser, nodeParserRef: node parser ref) =>
                        let val listParser = pmap (fn (tag, (arg, body)) => SList (tag, arg, body))
                                                  (andThen tagParser (andThen (opt argument)
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
    case (run sexpParser str) of
        (Success (node, _)) => Result node
     | _ => Fail "Bad parse"
end
