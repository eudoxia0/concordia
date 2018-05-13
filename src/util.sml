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

    fun readFileLines filepath (s, e) =
      String.concatWith "\n" (beforeLine (afterLine (stringLines (readFileToString filepath)) s) e)
    and stringLines s = String.tokens (fn c => c = #"\n") s
    and afterLine ls i = List.drop (ls, i - 1)
    and beforeLine ls i = List.take (ls, i - 1)

    fun member (x, nil) = false
      | member (x, y::ys) = (x=y) orelse member (x, ys)

    fun afterPrefix str prefix =
      if (String.isPrefix prefix str) then
          SOME (String.extract (str, (String.size prefix), NONE))
      else
          NONE
end
