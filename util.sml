signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    val isLeft : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool

    val readFileToString : string -> string
end

structure Util : UTIL = struct
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

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
end
