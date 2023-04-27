open Core
let p56 () =
  let to_digsum n =
    let ten = Bigint.of_int 10 in
    let rec loop m digsum = match Bigint.(m%ten, m/ten) with
      | (x,y) when Bigint.(x=zero && y=zero) -> digsum
      | (x,y) -> loop y Bigint.(x+digsum)
    in loop n Bigint.zero
  and hundred = Bigint.of_int 100
  in
  let rec loop smax a b = match (a,b) with
    | (x,_) when Bigint.(x=hundred) -> smax |> Bigint.to_int_exn
    | (x,y) when Bigint.(y=hundred) -> Bigint.(loop smax (x+one) (one))
    | (x,y) -> let ds = Bigint.(to_digsum (x**y)) in
      Bigint.(loop (if ds > smax then ds else smax) a (b+one))
  in
  Bigint.(loop zero one one)
