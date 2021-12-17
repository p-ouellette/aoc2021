fun cmdFromString s = let
      val [dir, dist] = String.tokens Char.isSpace s
      val i = valOf(Int.fromString dist)
       in case dir
            of "forward" => (i, 0)
             | "down" => (0, i)
             | "up" => (0, ~i)
      end

fun readCmd () = Option.map cmdFromString (TextIO.inputLine TextIO.stdIn)

fun part1 () = let
      fun loop (x, y) =
            case readCmd()
              of SOME(x', y') => loop(x + x', y + y')
               | NONE => x * y
       in loop(0, 0)
      end

fun part2 () = let
      fun loop (x, y, a) =
            case readCmd()
              of SOME(x', a') => loop(x + x', y + a*x', a + a')
               | NONE => x * y
       in loop(0, 0, 0)
      end

val _ = print(Int.toString(part2()) ^ "\n")
