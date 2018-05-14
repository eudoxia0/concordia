open Util

fun println str = print (str ^ "\n")

fun die str =
  let
  in
      println str;
      OS.Process.terminate OS.Process.failure
  end

fun getArgs prefix l = List.mapPartial (fn s => Util.afterPrefix s prefix) l

fun parseFile path =
  case (Parser.parseString (Util.readFileToString path)) of
      (Util.Result node) => node
    | (Util.Failure msg) => die msg

fun fileToHTML input output args =
  let val node = parseFile input
      and cssFiles = getArgs "--css=" args
      and jsFiles = getArgs "--js=" args
  in
      case Transform.parseDocument (CST.processIncludes node) of
          (Result doc) => let val html = HtmlBackend.htmlDocument doc cssFiles jsFiles
                          in
                              writeStringToFile output (HtmlGen.generate html)
                          end
        | (Failure msg) => println msg
  end

fun fileToTeX input output =
  let val node = parseFile input
  in
      case Transform.parseDocument (CST.processIncludes node) of
          (Result doc) => let val tex = TexBackend.texDocument doc
                          in
                              writeStringToFile output tex
                          end
        | (Failure msg) => println msg
  end

fun main () =
  let val args = CommandLine.arguments()
  in
      case args of
          "html"::rest => (case rest of
                               input::output::args => fileToHTML input output args
                             | _ => println "Usage: html <input> <output>")
        | "tex"::rest => (case rest of
                              input::output::_ => fileToTeX input output
                            | _ => println "Usage: tex <input> <output>")
        | _ => println "Unknown command"
  end

val _ = main ();
