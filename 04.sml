val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val ints = (map (valOf o Int.fromString)) o (String.tokens (not o Char.isDigit))
fun boards [] = []
  | boards rows  = List.take(rows, 5)::boards(List.drop(rows, 5))
val parseInput = (fn nums::rows => (nums, boards rows)) o (map ints) o readLines

fun initBoards boards = map (map (map (fn n => (n, false)))) boards
fun markBoard (b, n) = map (map (fn (x:int, false) => (x, x = n) | x => x)) b
fun markBoards (boards, n) = map (fn b => markBoard(b, n)) boards
fun cols ([]::_) = []
  | cols b = map hd b :: cols(map tl b)
fun hasRow b = List.exists (List.all (fn (_, m) => m)) b
fun isWin b = hasRow b orelse hasRow(cols b)
fun sumUnmarked row = foldl op+ 0 (map (fn (n, false) => n | _ => 0) row)
fun scoreBoard (b, n) = (foldl op+ 0 (map sumUnmarked b)) * n

fun part1 (nums, boards) = let
      fun loop (n::nums, boards) = let
            val boards' = markBoards(boards, n)
             in case List.find isWin boards'
                  of NONE => loop(nums, boards')
                   | SOME b => scoreBoard(b, n)
            end
       in loop(nums, initBoards boards)
      end

fun part2 (nums, boards) = let
      fun loop (n::nums, [b]) = let
            val b = markBoard(b, n)
             in if isWin b then scoreBoard(b, n) else loop(nums, [b])
            end
        | loop (n::nums, boards) = let
            val boards' = markBoards(boards, n)
             in loop(nums, List.filter (not o isWin) boards')
            end
       in loop(nums, initBoards boards)
      end

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val input = readInput "04.in"
val _ = print(Int.toString(part1 input)^"\n")
val _ = print(Int.toString(part2 input)^"\n")
