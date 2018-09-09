open Util

fun println str = print (str ^ "\n")

fun die str =
  let
  in
      println str;
      OS.Process.terminate OS.Process.failure
  end

fun getArgs prefix l =
    let val prefix' = "--" ^ prefix ^ "="
    in
        List.mapPartial (fn s => Util.afterPrefix s prefix') l
    end

fun getArg prefix l =
  case (getArgs prefix l) of
      (first::rest) => SOME first
    | _ => NONE

fun parseFile path =
  case (Parser.parseString (Util.readFileToString path)) of
      (Util.Result node) => node
    | (Util.Failure msg) => die msg

fun parseDocument path =
  case Transform.parseDocument (CST.processIncludes (parseFile path)) of
      (Result doc) => doc
    | (Failure msg) => die msg

fun fileToHTML input output args =
  let val doc = parseDocument input
      and cssFiles = getArgs "css" args
      and jsFiles = getArgs "js" args
      and texMacros = getArg "macros" args
  in
      let val texmacs = (case texMacros of
                             SOME path => TexMacros.parseMacroFile path
                           | _ => [])
      in
          let val html = HtmlBackend.htmlDocument doc cssFiles jsFiles texmacs
          in
              writeStringToFile output (HtmlGen.generate html)
      end
      end
  end

fun fileToTeX input output args =
  let val doc = parseDocument input
      and docclass = getArg "tex-document-class" args
      and docoptions = getArg "tex-document-options" args
      and mainFont = getArg "tex-main-font" args
  in
      let val tex = TexBackend.texDocument doc docclass docoptions mainFont
      in
          writeStringToFile output tex
      end
  end

fun main () =
  let val args = CommandLine.arguments()
  in
      case args of
          "html"::rest => (case rest of
                               input::output::args => fileToHTML input output args
                             | _ => println "Usage: html <input> <output>")
        | "tex"::rest => (case rest of
                              input::output::args => fileToTeX input output args
                            | _ => println "Usage: tex <input> <output>")
        | _ => println "Unknown command"
  end

val _ = main ();
