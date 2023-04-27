open Core
let p73 = fun () ->
  (* Version of p72 with adapted Farey F function *)
  let nmax = 12_000 in
  let ulim = (nmax/2)+1 in
  let mob = Array.create ~len:ulim 1
  and mask = Array.create ~len:ulim true
  and farey_f n = let (q,r) = (n/6, n%6) in
    q*(3*q-2+r) + (if r=5 then 1 else 0)
  in let rec sieve = fun r i ->
      if i >= ulim then r else
        match mask.(i) with
        | true when i<>1 -> (mob.(i) <- ~-1; inner_sieve i (i+i) 2; sieve (r-(farey_f (nmax/i))) (i+1))
        | _ -> sieve (r + mob.(i)*(farey_f (nmax/i))) (i+1)
  and inner_sieve = fun j0 j count ->
    match (j >= ulim, count) with
    | (false, x) when x=j0 -> (mask.(j) <- false; mob.(j) <- 0; inner_sieve j0 (j0+j) 1)
    | (false, _) -> (mask.(j) <- false; mob.(j) <- -mob.(j) (* Int32.(mob.(j)*m_one) *); inner_sieve j0 (j0+j) (count+1))
    | (true, _) -> ()
  in sieve 0 1
