structure Map = IntRedBlackMap
structure SCC = GraphSCCFn(struct
  type ord_key = int
  val compare = Int.compare
end)

val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val parseLine = (map (fn c => ord c - ord #"0")) o explode
val parseInput = (map parseLine) o readLines

fun riskLevel (h, adj) = if List.all (fn a => a > h) adj then h + 1 else 0

fun part1 m = let
      val empty = map (fn _ => 10) (hd m)
      val adj = ListPair.zip(empty::m, tl m @ [empty])
      in
        ListPair.foldlEq (fn (xs, adjv, acc) => let
          fun addRisk (x, ((w,e),(n,s)), acc) = acc + riskLevel(x, [w,e,n,s])
          val adjh = ListPair.zip(10::xs, tl xs @ [10])
          val adj = ListPair.zipEq(adjh, ListPair.zipEq adjv)
           in ListPair.foldlEq addRisk acc (xs, adj)
          end) 0 (m, adj)
      end

fun compSize (SCC.SIMPLE _) = 1
  | compSize (SCC.RECURSIVE l) = length l

fun part2 m = let
      val cols = length(hd m)
      val m = List.mapi (fn (r, xs) =>
              List.mapi (fn (_, 9) => NONE | (c, _) => SOME(r*cols + c)) xs) m
      val empty = map (fn _ => NONE) (hd m)
      val adj = ListPair.zip(empty::m, tl m @ [empty])
      val (nodes, follow) =
        ListPair.foldlEq (fn (xs, adjv, acc) => let
          fun f (n, adj, (nodes, m)) =
                (n::nodes, Map.insert(m, n, List.mapPartial (fn n => n) adj))
          fun f' (NONE, _, acc) = acc
            | f' (SOME node, ((w,e),(n,s)), acc) = f(node, [w,e,n,s], acc)
          val adjh = ListPair.zip(NONE::xs, tl xs @ [NONE])
          val adj = ListPair.zipEq(adjh, ListPair.zipEq adjv)
           in ListPair.foldlEq f' acc (xs, adj)
          end)
        ([], Map.empty) (m, adj)
      val comps = SCC.topOrder' {roots=nodes, follow=Fn.curry Map.lookup follow}
      val comps = ListMergeSort.sort op< (map compSize comps)
       in foldl op* 1 (List.take(comps, 3))
      end

fun readInput f = IOUtil.withInputFile(f, parseInput) TextIO.stdIn
val heightmap = readInput "09.in"
val _ = print(Int.toString(part1 heightmap)^"\n")
val _ = print(Int.toString(part2 heightmap)^"\n")
