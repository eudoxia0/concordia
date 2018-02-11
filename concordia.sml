fun fileToHTML input output =
  let val file = Util.readFileToString input
  in
      case (Parser.parseString file) of
          (Util.Result (CST.SList ("document", NONE, l))) =>
          let val doc = Transform.parseDocument' l
          in
              let val html = HtmlBackend.htmlDocument doc
              in
                  Util.writeStringToFile output (HtmlGen.generate html)
              end
          end
        | (Util.Failure msg) => print msg
        | _ => print "Unknown failure"
  end

fun main () =
  let val args = CommandLine.arguments()
  in
      case args of
          [input, output] => fileToHTML input output
        | _ => print "Bad input"
  end

val _ = main ();
