open Core
let p88 nmax =
  (* This one was difficult to speed up. Even with a few optimizations, it still takes ~6s AFTER compiling *)
  let sm k m lst = List.(fold lst ~init:(k-m) ~f:(+))
  and pm lst = List.(fold lst ~init:1 ~f:( * ))
  in let getmmax k =
       let rec findmmaxNewton k mn i = Float.(
           let mn1 = mn - ((k+mn)*(mn*(log 2.) - (log (k+mn))))/((k+mn)*(log 2.) - 1.) in
           if (abs (mn1-mn)) < 0.01 then (if (mn1-(round_down mn1)) > 0.99 then (to_int (round_up mn1)) else (to_int (round_down mn1)))
           else (match i with
               | 10 -> mn1 |> to_int
               | (_ : int) -> findmmaxNewton k mn1 Int.(i+1))
         )
       in findmmaxNewton (float k) (sqrt (float k)) 0
  in let rec dloop k m lst nmin i =
       let asum = sm k m lst and psum = pm lst
       in let n1 = (if (asum = psum && asum < nmin) then asum else nmin)
       and i1 = (if (asum <= psum) || (asum >= nmin) then i+1 else 0)
       in match (i1, lst) with
       | (_ : int * int list) when m <= 1 -> n1
       | (0, car::cdr) -> dloop k m ((car + 1) :: cdr) n1 i1
       | (j, (_ : int list)) when j>=k -> n1
       | (j, (_ : int list)) when j>=m -> if m = 2 then n1 else
           (let a1 = ((k + 2*(m-2) - m + 1)/((Int.pow 2 (m-2)) - 1))
            in (dloop k (m-1) (a1 :: (List.init (m-2) ~f:(fun (_ : int) -> 2))) n1 0))
       | (j, l) -> (dloop k m
                      List.(nth_exn l j |> (+) 1
                            |> (fun x -> mapi l ~f:(fun ind y -> (if ind <= j then x else y))))
                      n1 i1)
  in let findn k = getmmax k |> (fun m -> dloop k m (List.init m ~f:(fun (_ : int) -> 2)) Int.max_value 0)
  in let nsum kmax =
       let rec kloop k acc = match (k > kmax, acc) with
         | (true, (nsum, (_ : Core.Int.Set.t))) -> nsum
         | (false, (nsum, nset)) -> let n = findn k in
           (if Int.Set.mem nset n then kloop (k+1) acc else kloop (k+1) ((nsum+n, Int.Set.add nset n)))
       in kloop 2 (0, Int.Set.empty)
  in nsum nmax
