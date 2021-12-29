structure PM = RedBlackMapFn(struct
  type ord_key = int * int
  fun compare ((x, y), (x', y')) =
        case Int.compare(x, x')
          of EQUAL => Int.compare(y, y')
           | order => order
end)

val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val parseLine = (fn [x, y, x', y'] => ((x, y), (x', y')))
              o (map (valOf o Int.fromString))
              o (String.tokens (not o Char.isDigit))
val parseInput = (map parseLine) o readLines

fun range (m, n, s) = if m = n then [m] else m::range(m+s, n, s)
fun points (((x, y), (x', y')), diag) =
      case (Int.sign(x'-x), Int.sign(y'-y), diag)
        of (0, s, _) => map (fn y => (x, y)) (range(y, y', s))
         | (s, 0, _) => map (fn x => (x, y)) (range(x, x', s))
         | (sx, sy, true) => ListPair.zipEq(range(x, x', sx), range(y, y', sy))
         | _ => []
fun addPoints (m, ps) = foldl (fn (p, m) => PM.insertWith op+ (m, p, 1)) m ps
fun countOverlaps diag lines = let
      val ps = foldl (fn (l, m) => addPoints(m, points(l, diag))) PM.empty lines
       in PM.foldl (fn (1, n) => n | (_, n) => n+1) 0 ps
      end

val part1 = countOverlaps false
val part2 = countOverlaps true

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val lines = readInput "05.in"
val _ = print(Int.toString(part1 lines)^"\n")
val _ = print(Int.toString(part2 lines)^"\n")
