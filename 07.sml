fun alignCost ps x = foldl (fn (p, c) => c + Int.abs(p-x)) 0 ps
fun tri n = n*(n+1) div 2
fun alignCost' ps x = foldl (fn (p, c) => c + tri(Int.abs(p-x))) 0 ps
fun min (x::xs) = foldl Int.min x xs
fun max (x::xs) = foldl Int.max x xs

fun part1 ps = min(map (alignCost ps) ps)
fun part2 ps = min(List.tabulate(max ps, alignCost' ps))

val parseInts = (map (valOf o Int.fromString)) o (String.tokens Char.isPunct)
val parseInput = parseInts o TextIO.inputAll
fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val positions = readInput "07.in"
val _ = print(Int.toString(part1 positions)^"\n")
val _ = print(Int.toString(part2 positions)^"\n")
