open Core

let p21 = fun nmax ->
  let nlist = List.range 2 nmax
  in let get_facsum = fun n ->
      let ulim = n |> float |> sqrt |> int_of_float
      in let rec scan = fun fsum flist ->
          match flist with
          | car::_ when car >= ulim -> if car*car = n then fsum + car else fsum
          | car::cdr when n mod car = 0 -> scan (fsum + car + (n/car)) cdr
          | _::cdr -> scan fsum cdr
          | [] -> fsum
      in scan 1 nlist
  in let test_amicable = fun n ->
      match get_facsum n with
      | x when x <= n -> 0
      | x -> (fun y -> match get_facsum y with
          | x when x = n -> y + n
          | _ -> 0) x
  in List.fold nlist ~init:0 ~f:(fun acc n -> acc + (test_amicable n))
