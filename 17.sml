val target = {x1=269, x2=292, y1= ~68, y2= ~44}

(* Assume Vx = 0 when we hit the target. *)
fun part1 () = let
      val vy = ~(#y1 target) - 1
       in vy*(vy+1) div 2
      end

fun hitsTarget v = let
      fun overshot (x, y) = x > #x2 target orelse y < #y1 target
      fun enteredTarged (x, y) = x >= #x1 target andalso y <= #y2 target
      fun step ((x, y), (vx, vy)) = ((x+vx, y+vy), (vx-Int.sign vx, vy-1))
      fun loop (p, v) =
            not(overshot p) andalso (enteredTarged p orelse loop(step(p, v)))
       in loop((0, 0), v)
      end

fun part2 () = let
      val vxMax = #x2 target
      val vyMin = #y1 target
      val vyMax = ~vyMin - 1
      fun loop (vx, vy) =
            if vx < 0 then 0
            else if vy < vyMin then loop(vx-1, vyMax)
            else
              if hitsTarget(vx, vy) then 1 + loop(vx, vy-1) else loop(vx, vy-1)
       in loop(vxMax, vyMax)
      end

val _ = print(Int.toString(part1()) ^ "\n")
val _ = print(Int.toString(part2()) ^ "\n")
