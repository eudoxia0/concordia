structure TexMacros :> TEX_MACROS = struct
  datatype macro = Macro of string * int * string

  fun macroName (Macro (n, _, _)) = n
  fun macroArity (Macro (_, a, _)) = a
  fun macroDefinition (Macro (_, _, d)) = d

  fun parseLine line =
    let fun delim c = c = #" "
    in
        case (String.tokens delim line) of
            (name::ar::rest) => parse name ar (String.concatWith " " rest)
          | _ => NONE
    end
  and parse name ar def =
      case (Int.fromString ar) of
          SOME i => SOME (Macro (name, i, def))
        | NONE => NONE

  fun toString (Macro (name, arity, def)) =
    name ^ " " ^ (Int.toString arity) ^ " " ^ def

  fun mathJaxConfig ms =
    let val prefix = String.concatWith "\n" [
                "MathJax.Hub.Config({",
                "  jax: ['input/TeX', 'output/SVG'],",
                "  messageStyle: 'none',",
                "  //displayAlign: 'left',",
                "  //displayIndent: '4em',",
                "  tex2jax: {",
                "    inlineMath: [['$', '$']],",
                "    displayMath: [['\\\\(','\\\\)']],",
                "    processEscapes: true",
                "  },",
                "  TeX: {",
                "    Macros: {"
            ]
        and suffix = String.concatWith "\n" [
                "    },",
                "    equationNumbers: { autoNumber: 'AMS' }",
                "  }",
                "});",
                "",
                "MathJax.Hub.Register.StartupHook('TeX Jax Ready', function () {",
                "  MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros, {",
                "    cancel: ['Extension', 'cancel'],",
                "  });",
                "});"
            ]
    in
        let fun macJS (Macro (n, a, d)) =
              "      \"" ^ n ^ "\": [\"" ^ (escapeTex d) ^ "\", " ^ (Int.toString a) ^ "]"
            and escapeTex s = String.translate (fn c => escapeChar c) s
            and escapeChar #"\\" = "\\\\"
              | escapeChar c = str c
        in
            let val macS = (String.concatWith ",\n" (map macJS ms))
            in
                String.concatWith "\n" [prefix, macS, suffix]
            end
        end
    end

  fun parseMacroFile path =
    List.mapPartial parseLine (Util.readFileLines path)
end
