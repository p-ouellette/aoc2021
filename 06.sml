fun initState ages = List.tabulate(9, fn n => length(List.filter (Fn.equal n) ages))
fun nextDay (n::counts) = List.update(counts, 6, List.nth(counts, 6) + n) @ [n]
fun popAfterDays n = (foldl op+ 0) o (Fn.repeat n nextDay) o initState

val part1 = popAfterDays 80
val part2 = popAfterDays 256

val parseInts = (map (valOf o Int.fromString)) o (String.tokens Char.isPunct)
val parseInput = parseInts o TextIO.inputAll
fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val ages = readInput "06.in"
val _ = print(Int.toString(part1 ages)^"\n")
val _ = print(Int.toString(part2 ages)^"\n")
