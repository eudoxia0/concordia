signature UTIL = sig
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b
end

structure Util : UTIL = struct
    datatype ('a, 'b) either = Left of 'a
                             | Right of 'b
end
