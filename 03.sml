val toInt = foldl (fn (b, n) => n*2 + b) 0

fun part1 (x::xs) = let
      val half = length(x::xs) div 2
      val counts = foldl (ListPair.mapEq op+) x xs
      val gamma = map (fn n => if n > half then 1 else 0) counts
      val epsilon = map (fn 0 => 1 | _ => 0) gamma
       in toInt gamma * toInt epsilon
      end

fun filterNums f nums = let
      fun loop ([n], _) = n
        | loop (nums, i) = let
            val (tru, fls) = List.partition (fn n => List.nth(n, i) = 1) nums
            val nums' = if f(length tru, length fls) then tru else fls
             in loop(nums', i+1)
            end
       in loop(nums, 0)
      end

fun part2 nums = toInt(filterNums op>= nums) * toInt(filterNums op< nums)

val parseNum = (map (fn c => ord c - ord #"0")) o explode
val parseInput = (map parseNum) o (String.tokens Char.isSpace) o TextIO.inputAll
fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val nums = readInput "03.in"
val _ = print(Int.toString(part1 nums)^"\n")
val _ = print(Int.toString(part2 nums)^"\n")
