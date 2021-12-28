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

fun range f (m, n, s) = f m :: (if m = n then [] else range f (m+s, n, s))
fun points ((x, y), (x', y')) = let
      val (xs, ys) =
        case (Int.sign(x'-x), Int.sign(y'-y))
          of (0, s) => (range (fn _ => x) (y, y', s), range Fn.id (y, y', s))
           | (s, 0) => (range Fn.id (x, x', s), range (fn _ => y) (x, x', s))
           | (sx, sy) => (range Fn.id (x, x', sx), range Fn.id (y, y', sy))
       in ListPair.zipEq(xs, ys)
      end
fun addPoints (ps, m) = foldl (fn (p, m) => PM.insertWith op+ (m, p, 1)) m ps

fun part1 lines = let
      fun points' (l as ((x, y), (x', y'))) =
            if x = x' orelse y = y' then points l else []
      val ps = foldl (fn (l, m) => addPoints(points' l, m)) PM.empty lines
       in PM.foldl (fn (1, n) => n | (_, n) => n+1) 0 ps
      end

fun part2 lines = let
      val ps = foldl (fn (l, m) => addPoints(points l, m)) PM.empty lines
       in PM.foldl (fn (1, n) => n | (_, n) => n+1) 0 ps
      end

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val lines = readInput "05.in"
val _ = print(Int.toString(part1 lines)^"\n")
val _ = print(Int.toString(part2 lines)^"\n")
