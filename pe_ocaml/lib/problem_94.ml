open Core

let p94 nmax =
  let mmax = nmax + 1 |> float |> ( *. ) (1. /. 6.) |> sqrt |> int_of_float
  and eratosthenes = fun ?(rev = false) nmax ->
    (* Sieve of Eratosthenes implementation *)
    let ulim = nmax |> float |> sqrt |> int_of_float
    and i_nil = fun _ -> ()
    in let mask = Array.create ~len:nmax true
    in let rec sieve = fun primes i ->
        if i >= nmax then (if rev then primes else List.rev primes) else
          match mask.(i) with
          | true when i > ulim -> sieve (i::primes) (i+1)
          | true -> (inner_sieve i i*i |> i_nil;
                     sieve (i::primes) (i+1))
          | false -> sieve primes (i+1)
    and inner_sieve = fun j0 j ->
      match j >= nmax with
      | true -> 0
      | false -> (mask.(j) <- false; inner_sieve j0 (j0+j))
    in sieve [] 2
  in let primes = eratosthenes (mmax |> float |> sqrt |> int_of_float)
  in let coprime_p m n =
       if n = 1 then true else
         let ulim = min (m |> float |> sqrt |> int_of_float) n
         in List.fold_until primes ~init:true ~finish:(fun x -> x)
           ~f:(fun _ p -> match (m mod p, n mod p) with
               | (0,0) -> (Stop false)
               | _ when p > ulim -> (Stop true)
               | _ -> (Continue true))
  and is_square n =
    n |> float |> sqrt |> int_of_float |> fun x -> (x*x - n) = 0
  in let get_n_cand_m2n2 m =
       let m2pm1 = if (m*m + 1) mod 3 = 0 then (m*m + 1)/3 else (m*m - 1)/3 in
       match is_square m2pm1 with
       | true -> Some (m, m2pm1 |> float |> sqrt |> int_of_float)
       | false -> None
  and get_n_cand_2mn m =
    let m2_3 = 3*m*m in
    if is_square (m2_3 + 1) then
      Some (m, 2*m - (m2_3 + 1 |> float |> sqrt |> int_of_float))
    else
      (if is_square (m2_3 - 1) then
         Some (m, 2*m - (m2_3 - 1 |> float |> sqrt |> int_of_float))
       else None)
  in let get_perim m =
       (+)
         (match get_n_cand_m2n2 m with
          | Some (x, y) -> if coprime_p x y then 4*x*x else 0
          | None -> 0)
         (match get_n_cand_2mn m with
          | Some (x, y) -> if coprime_p x y then 2*(x+y)*(x+y) else 0
          | None -> 0)
  in let rec loop psum m =
       match m with
       | x when x > mmax -> psum
       | x -> loop (psum + (get_perim x)) (x+1)
  in loop 0 2
