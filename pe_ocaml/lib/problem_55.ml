open Core
let p55 () =
  let reverse n =
    let rec loop m nrev = match (m%10, m/10) with
      | (0,0) -> nrev
      | (x,y) -> loop y (nrev*10 + x)
    in loop n 0
  and reverse_big n = Bigint.(to_string n |> String.rev |> of_string)
  in
  let test_lychrel n =
    let biglim = Int.max_value / 10 in
    let rec loop m i = match reverse m with
      | _ when i >= 50 -> Some n
      | x when x = m -> None
      | x when x > biglim ->
        loop_big Bigint.(of_int x) i
      | x -> loop (m+x) (i+1)
    and loop_big m i = match reverse_big m with
      | _ when i >= 50 -> Some n
      | x when Bigint.(x = m) -> printf "%s" Bigint.(to_string x); None
      | x -> loop_big Bigint.(m+x) (i+1)
    in loop (n+(reverse n)) 1
  in
  List.(range 1 10000 |> filter_map ~f:test_lychrel |> length)
