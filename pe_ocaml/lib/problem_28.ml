open Core

let p28 () =
  let dmax = 1001*1001
  in let rec spiral_sum sum dprev n =
       match dprev + 2*(n/4 + 1) with
       | _ when dprev >= dmax -> sum
       | x -> spiral_sum (sum + x) x (n+1)
  in spiral_sum 1 1 0
