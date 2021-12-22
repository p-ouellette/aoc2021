fun cmdParser getc =
      ParserComb.wrap(Scan.scanf "%s %d\n", fn [Scan.STR s, Scan.INT i] =>
        case (s, i)
          of ("forward", i) => (i, 0)
           | ("down", i) => (0, i)
           | ("up", i) => (0, ~i)) getc

fun parser getc = ParserComb.zeroOrMore cmdParser getc

fun part1 cmds = let
      fun step ((x', y'), (x, y)) = (x + x', y + y')
       in op* (foldl step (0, 0) cmds)
      end

fun part2 cmds = let
      fun step ((x', a'), (x, y, a)) = (x + x', y + a*x', a + a')
      val (x, y, _) = foldl step (0, 0, 0) cmds
       in x * y
      end

fun readInput f = IOUtil.withInputFile(f, TextIO.scanStream parser) TextIO.stdIn
val cmds = valOf(readInput "02.in")
val _ = print(Int.toString(part1 cmds)^"\n")
val _ = print(Int.toString(part2 cmds)^"\n")
