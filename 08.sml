structure Set = IntListSet

val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val wireNums = map (fn c => ord c - ord #"a") o explode
val parseLine = (fn [patterns, output] => (patterns, output))
              o (map (map wireNums o (String.tokens Char.isSpace)))
              o (String.tokens (fn c => c = #"|"))
val parseInput = (map parseLine) o readLines

fun isUnique p = case length p of 2 => 1 | 3 => 1 | 4 => 1 | 7 => 1 | _ => 0
fun countUnique patterns = foldl (fn (p, n) => n + isUnique p) 0 patterns
fun part1 entries = foldl (fn ((_, out), n) => n + countUnique out) 0 entries

val digits = map Set.fromList
      [[0, 1, 2, 4, 5, 6],
       [2, 5],
       [0, 2, 3, 4, 6],
       [0, 2, 3, 5, 6],
       [1, 2, 3, 5],
       [0, 1, 3, 5, 6],
       [0, 1, 3, 4, 5, 6],
       [0, 2, 5],
       [0, 1, 2, 3, 4, 5, 6],
       [0, 1, 2, 3, 5, 6]
      ]
fun isWireOn (wire:int, pat) = isSome(List.find (fn s => s = wire) pat)
fun segmentOfWire patterns = let
      fun count wire =
            foldl (fn (p, n) => if isWireOn(wire, p) then n+1 else n) 0 patterns
      fun getUnique len = valOf(List.find (fn p => length p = len) patterns)
      val [one, four] = map getUnique [2, 4]
      fun segment wire =
            case count wire
              of 4 => 4
               | 6 => 1
               | 7 => if isWireOn(wire, four) then 3 else 6
               | 8 => if isWireOn(wire, one) then 2 else 0
               | 9 => 5
      val segments = List.tabulate(7, segment)
       in fn wire => List.nth(segments, wire)
      end
fun displayToNum disp =
      #1(valOf(List.findi (fn (_, d) => Set.equal(disp, d)) digits))
fun decode (patterns, out) = let
      val segmentOfWire = segmentOfWire patterns
      fun decodeNum pat = displayToNum(Set.fromList(map segmentOfWire pat))
       in foldl (fn (p, n) => n*10 + decodeNum p) 0 out
      end
fun part2 entries = foldl (fn (e, n) => n + decode e) 0 entries

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val entries = readInput "08.in"
val _ = print(Int.toString(part1 entries)^"\n")
val _ = print(Int.toString(part2 entries)^"\n")
