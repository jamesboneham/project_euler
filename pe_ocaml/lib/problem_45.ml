open Core
let p45 () =
  let test_p n =
    let nsqrt = Float.(of_int Int.(24*n+1) |> sqrt |> iround_nearest_exn)
    in nsqrt*nsqrt=24*n+1 && nsqrt mod 6 = 5
  in
  let find_next_pent n0 h0 =
    let rec loop n h = match test_p h with
      | true -> h
      | false -> loop (n+1) (h+4*n+1)
    in loop (n0+1) (h0+4*n0+1)
  in
  find_next_pent 143 40755
