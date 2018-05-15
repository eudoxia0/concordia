signature TEX_MACROS = sig
  type macro

  val macroName : macro -> string
  val macroArity : macro -> int
  val macroDefinition : macro -> string

  val toString : macro -> string

  val mathJaxConfig : macro list -> string

  val parseMacroFile : string -> macro list
end
