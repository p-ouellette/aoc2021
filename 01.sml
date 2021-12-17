fun readInt () = TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn

fun part1 () = let
      fun loop (incr, prev) =
            case readInt()
              of SOME i => loop(if i > prev then incr + 1 else incr, i)
               | NONE => incr
       in loop(0, valOf(readInt()))
      end

fun part2 () = let
      fun loop (incr, [a, b, c]) =
            case readInt()
              of SOME d => let
                   val incr = if b+c+d > a+b+c then incr + 1 else incr
                    in loop(incr, [b, c, d])
                   end
               | NONE => incr
       in loop(0, map valOf [readInt(), readInt(), readInt()])
      end

val _ = print(Int.toString(part2()) ^ "\n")
