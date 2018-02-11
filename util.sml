signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    datatype 'a result = Result of 'a
                       | Failure of string

    val isLeft : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool

    val readFileToString : string -> string
    val writeStringToFile : string -> string -> unit

    val member : (''a * ''a list) -> bool

    val afterPrefix : string -> string -> string option
end

structure Util : UTIL = struct
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    datatype 'a result = Result of 'a
                       | Failure of string

    fun isLeft (Left _) = true
      | isLeft _ = false

    fun isRight (Left _) = false
      | isRight _ = true

    fun readFileToString filepath =
      let val stream = TextIO.openIn filepath
          fun loop stream =
            case TextIO.inputLine stream of
                SOME line => line :: loop stream
              | NONE      => []
      in
          String.concat (loop stream before TextIO.closeIn stream)
      end

    fun writeStringToFile filepath str =
      let val stream = TextIO.openOut filepath
      in
          TextIO.output (stream, str) handle e => (TextIO.closeOut stream; raise e);
          TextIO.closeOut stream
      end

    fun member (x, nil) = false
      | member (x, y::ys) = (x=y) orelse member (x, ys)

    fun afterPrefix str prefix =
      if (String.isPrefix prefix str) then
          SOME (String.extract (str, (String.size prefix), NONE))
      else
          NONE
end
