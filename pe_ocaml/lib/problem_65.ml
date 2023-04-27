open Core
let p65 () =
  let (zero, one, two, three) = Bigint.(zero, one, of_int 2, of_int 3) in
  let rec gcd a b = Bigint.(match b with | x when x = zero -> a | _ -> gcd b (a % b))
  in
  let merge a (n,d) =
    let d1 = Bigint.(a*d+n) in
    let fac = gcd d d1 in
    Bigint.(d/fac, d1/fac)
  and e_cons nmax = match nmax with
    | x when x > 2 ->
      zero::two::one::List.(init (nmax-2) ~f:(fun i ->
          (if i%3 = 0 then Bigint.(((of_int i)/three + one)*two) else one)))
    | 2 -> [zero;two;one]
    | 1 -> [zero;two]
    | _ -> []
  in
  let get_conv n = match List.rev @@ e_cons n with
    | d1::cdr ->
      List.fold cdr ~init:(one,d1) ~f:(fun conv a -> merge a conv)
      |> fst
      |> Bigint.to_string
      |> String.to_list
      |> List.map ~f:(fun c -> c |> Char.to_string |> Int.of_string)
      |> List.reduce_exn ~f:(+)
    | _ -> 0
  in get_conv 100
