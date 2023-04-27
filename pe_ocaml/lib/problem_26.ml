open Core

let p26 nmax =
  let getnrepeats n =
    let pow10 = Float.(n |> of_int |> log10 |> round_up |> ( ** ) 10. |> to_int)
    in let rec nrepeats rem ndigs =
         match (10*rem) with
         | 0 -> 0
         | x when x = pow10 -> ndigs
         | x -> nrepeats (x mod n) (ndigs + 1)
    in nrepeats (pow10 mod n) 1
  in List.fold_until (Utils.eratosthenes ~rev:true nmax) ~init:(0,0) ~finish:(fun x -> x)
    ~f:(fun max_cycle prime -> match (max_cycle, getnrepeats prime) with
        | ((_, cmax),x) when cmax < prime -> Continue (if x > cmax then (prime,x) else max_cycle)
        | _ -> Stop max_cycle)
   |> fst
