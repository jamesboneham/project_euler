open Core
let p97 () =
  let tendig = Int.pow 10 10
  in let tendigfilter n = n mod tendig
  in let rec loop pwr n = match pwr with
      | 7830457 -> n + 1
      | x -> loop (x+1) (tendigfilter (2*n))
  in loop 0 28433
