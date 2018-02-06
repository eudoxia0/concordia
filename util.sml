signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    val isLeft : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool
end

structure Util : UTIL = struct
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    fun isLeft (Left _) = true
      | isLeft _ = false

    fun isRight (Left _) = false
      | isRight _ = true
end
