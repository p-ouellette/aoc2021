fun parser getc = ParserComb.zeroOrMore(Int.scan StringCvt.DEC) getc

fun part1 depths = let
      fun loop (a::b::xs, n) = loop(b::xs, if b > a then n+1 else n)
        | loop (_, n) = n
       in loop(depths, 0)
      end

fun part2 depths = let
      fun loop (l as a::_::_::b::_, n) = loop(tl l, if b > a then n+1 else n)
        | loop (_, n) = n
       in loop(depths, 0)
      end

fun readInput f = IOUtil.withInputFile(f, TextIO.scanStream parser) TextIO.stdIn
val depths = valOf(readInput "01.in")
val _ = print(Int.toString(part1 depths)^"\n")
val _ = print(Int.toString(part2 depths)^"\n")
