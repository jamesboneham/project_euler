open Core
let p48 () =
  let tenten = Int.pow 10 10
  in
  let self_pow x =
    Bigint.(
      let m = of_int tenten
      and two = of_int 2
      and y = of_int x
      in
      let rec loop r b e = match e > zero with
        | false -> r
        | true -> loop ((if e % two = one then r*b else r) % m) ((b*b) % m) (e / two)
      in loop one y y |> to_int_exn
    )
  in
  List.(range 1 1001 |> fold ~init:0 ~f:(fun acc x -> (acc + (self_pow x)))) % tenten
