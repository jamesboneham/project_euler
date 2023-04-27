open Core

let p75 nmax =
  let mlim = Int.(((1+2*nmax) |> to_float |> sqrt |> of_float)-1)/2
  and counts = Array.create ~len:nmax 0
  in let rec kloop lprim l =
       match l >= nmax with
       | true -> ()
       | false -> (counts.(l) <- counts.(l) + 1); kloop lprim (l + lprim)
  and gcd = function (a, 0) -> a
                   | (a, b) -> gcd (b, a mod b)
  in let rec mnloop = fun m n ->
      match (m,n,2*m*(m+n)) with
      | (x,_,_) when x > mlim -> Array.count counts ~f:(function 1 -> true | _ -> false)
      | (x,y,_) when y >= x -> mnloop (x+1) 1
      | (x,y,z) -> ((if (m*n mod 2 = 0) && (gcd (m,n) = 1) then kloop z z else ());
                    mnloop x (y+1))
  in mnloop 2 1
