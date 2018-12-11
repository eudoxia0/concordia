signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b

    datatype 'a result = Result of 'a
                       | Failure of string

    val isLeft : ('a, 'b) either -> bool
    val isRight : ('a, 'b) either -> bool

    val readFileToString : string -> string
    val writeStringToFile : string -> string -> unit

    val readFileLines : string -> string list
    val readFileRange : string -> (int * int) -> string

    val member : (''a * ''a list) -> bool

    (* If the second string is a prefix of the first string, return the first
       string with the prefix removed *)
    val afterPrefix : string -> string -> string option
end
