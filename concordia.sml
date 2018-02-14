fun println str = print (str ^ "\n")

fun getArgs prefix l = List.mapPartial (fn s => Util.afterPrefix s prefix) l

fun fileToHTML input output args =
  let val file = Util.readFileToString input
      and cssFiles = getArgs "--css=" args
      and jsFiles = getArgs "--js=" args
  in
      case (Parser.parseString file) of
          (Util.Result (CST.SList ("document", NONE, l))) =>
          (case (Transform.parseDocument l) of
               (Util.Result doc) => let val html = HtmlBackend.htmlDocument doc cssFiles jsFiles
                                    in
                                        Util.writeStringToFile output (HtmlGen.generate html)
                                    end
             | (Util.Failure msg) => println msg)
        | (Util.Failure msg) => println msg
        | _ => println "Unknown failure"
  end

fun main () =
  let val args = CommandLine.arguments()
  in
      case args of
          "html"::rest => (case rest of
                               input::output::args => fileToHTML input output args
                             | _ => println "Usage: html <input> <output>")
       | _ => println "Unknown command"
  end

val _ = main ();
