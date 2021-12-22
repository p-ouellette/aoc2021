(* Assume Vx = 0 when we hit the target. *)
fun part1 {xmin, xmax, ymin, ymax} = let
      val vy = ~ymin - 1
       in vy*(vy+1) div 2
      end

fun hitsTarget ({xmin, xmax, ymin, ymax}, v) = let
      fun overshot (x, y) = x > xmax orelse y < ymin
      fun enteredTarged (x, y) = x >= xmin andalso y <= ymax
      fun step ((x, y), (vx, vy)) = ((x+vx, y+vy), (vx-Int.sign vx, vy-1))
      fun loop (p, v) =
            not(overshot p) andalso (enteredTarged p orelse loop(step(p, v)))
       in loop((0, 0), v)
      end

fun part2 (t as {xmin, xmax, ymin, ymax}) = let
      val vyMax = ~ymin - 1
      fun loop (vx, vy, n) =
            if vx < 0 then n
            else if vy < ymin then loop(vx-1, vyMax, n)
            else loop(vx, vy-1, if hitsTarget(t, (vx, vy)) then n+1 else n)
       in loop(xmax, vyMax, 0)
      end

val target = {xmin=269, xmax=292, ymin= ~68, ymax= ~44}
val _ = print(Int.toString(part1 target)^"\n")
val _ = print(Int.toString(part2 target)^"\n")
