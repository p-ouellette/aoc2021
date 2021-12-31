val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val parseLine = (fn [signals, output] => (signals, output))
              o (map (String.tokens Char.isSpace))
              o (String.tokens (fn c => c = #"|"))
val parseInput = (map parseLine) o readLines

val countUnique = (foldl op+ 0) o
      (map ((fn 2 => 1 | 3 => 1 | 4 => 1 | 7 => 1 | _ => 0) o String.size))

fun part1 entries = foldl (fn ((_, out), n) => n + countUnique out) 0 entries

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val entries = readInput "08.in"
val _ = print(Int.toString(part1 entries)^"\n")
(*
val _ = print(Int.toString(part2 entries)^"\n")
*)
