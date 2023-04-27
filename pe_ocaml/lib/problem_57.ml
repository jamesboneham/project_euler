open Core
let p57 () =
  let nreps = 1000
  and ten = Bigint.(of_int 10)
  and two = Bigint.(of_int 2)
  in
  let rec loop counter pwr10 d1 d0 i =
    if i >= nreps then counter
    else let d = Bigint.(two*d1 + d0) in
      match Bigint.(d>=pwr10,d1+d>=pwr10) with
      | (true, true) -> loop counter Bigint.(pwr10*ten) d1 d0 i
      | (false, true) -> loop (counter+1) Bigint.(pwr10*ten) d d1 (i+1)
      | _ -> loop counter pwr10 d d1 (i+1)
  in Bigint.(loop 0 one one zero 0)
