open Core
let p78 = fun () ->
  let nmax = 60_000
  and ndiv = 1_000_000
  in let kmax = Float.(to_int (((sqrt (24.*(of_int nmax) + 1.)) + 1.)/6.))
  in let pents = List.init ~f:(fun k -> ((if k/2 mod 2 = 0 then (+) else (-)),
                                         ((((k+2)/2)*(3*((k+2)/2) - 1 + 2*(k mod 2)))/2))) (2*kmax)
  in let parts = Array.create ~len:(nmax+1) 1
  in let getp n = List.fold_until pents ~init:0 ~finish:(fun x->x)
         ~f:(fun acc (porm, pent) -> match pent with
             | x when x < n -> Continue (porm acc parts.(n-x))
             | x when x = n -> Stop (porm acc 1)
             | _ -> Stop acc)
  in let rec fillparts n = match (n, (getp n) mod ndiv) with
      | (x, y) when (y = 0) || (x = nmax) -> parts.(n) <- y; x
      | (x, y) -> parts.(n) <- y; fillparts (x+1)
  in ((fillparts 1) : int)
