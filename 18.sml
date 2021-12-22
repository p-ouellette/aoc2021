structure P = ParserComb

datatype sfnum = Num of int
               | Pair of sfnum * sfnum

val +> = P.seq
infixr 3 +>

fun numParser getc = P.wrap(Int.scan StringCvt.DEC, Num) getc
fun sfnumParser getc = P.or(pairParser, numParser) getc
and pairParser getc = P.wrap(
      P.char #"[" +> sfnumParser +> P.char #"," +> sfnumParser +> P.char #"]",
      fn (_, (a, (_, (b, _)))) => Pair(a, b)) getc
fun parser getc = P.zeroOrMore (P.skipBefore Char.isSpace sfnumParser) getc

fun explodeNum n = let
      fun addl (Num n, i) = Num(n + i)
        | addl (Pair(a, b), i) = Pair(addl(a, i), b)
      fun addr (Num n, i) = Num(n + i)
        | addr (Pair(a, b), i) = Pair(a, addr(b, i))

      fun addLeft (a, (b, SOME(SOME l, r))) = (addr(a, l), b, SOME(NONE, r))
        | addLeft (a, (b, lr)) = (a, b, lr)
      fun addRight ((a, SOME(l, SOME r)), b) = (a, addl(b, r), SOME(l, NONE))
        | addRight ((a, lr), b) = (a, b, lr)

      fun explode (Num n, _) = (Num n, NONE)
        | explode (Pair(Num a, Num b), 4) = (Num 0, SOME(SOME a, SOME b))
        | explode (Pair(a, b), depth) = let
            val (a, b, lr) =
              case addRight(explode(a, depth+1), b)
                of (_, _, NONE) => addLeft(a, explode(b, depth+1))
                 | e => e
             in (Pair(a, b), lr)
            end
      val (n, lr) = explode(n, 0)
       in (n, isSome lr)
      end

fun splitNum (Num n) =
      if n > 9
      then (Pair(Num(n div 2), Num((n+1) div 2)), true)
      else (Num n, false)
  | splitNum (Pair(a, b)) = let
      val (a, split) = splitNum a
      val (b, split) = if split then (b, split) else splitNum b
       in (Pair(a, b), split)
      end

fun reduceNum n =
      case explodeNum n
        of (n, true) => reduceNum n
         | _ =>
      case splitNum n
        of (n, true) => reduceNum n
         | _ => n

fun addNum (a, b) = reduceNum(Pair(a, b))

fun magnitude (Num n) = n
  | magnitude (Pair(a, b)) = 3*magnitude a + 2*magnitude b

fun part1 (x::xs) = magnitude(foldl (fn (a, b) => addNum(b, a)) x xs)

fun part2 cmds = let
      fun loop ([], _, m) = m
        | loop (x::xs, [], m) = loop(xs, cmds, m)
        | loop (x::xs, y::ys, m) =
            if x <> y then
              loop(x::xs, ys, Int.max(m, magnitude(addNum(x, y))))
            else loop(x::xs, ys, m)
       in loop(cmds, cmds, 0)
      end

fun readInput f = IOUtil.withInputFile(f, TextIO.scanStream parser) TextIO.stdIn
val nums = valOf(readInput "18.in")
val _ = print(Int.toString(part1 nums)^"\n")
val _ = print(Int.toString(part2 nums)^"\n")
