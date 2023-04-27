let p12 = fun ndivs ->
  let eratosthenes = fun nmax ->
    let ulim = nmax |> float |> sqrt |> int_of_float
    and i_nil = fun _ -> ()
    in let mask = Array.make nmax true
    in let rec sieve = fun primes i ->
        if i >= nmax then List.rev primes else
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
  in let rec prime_fact = fun n nfacs ai plist ->
      match plist with
      | _ when n = 1 -> nfacs*ai
      | hd::_ when n mod hd = 0 -> prime_fact (n/hd) nfacs (ai+1) plist
      | hd::tl when n mod hd != 0 -> prime_fact n (nfacs*ai) 1 tl
      | _ -> -1
  and trinext = fun ndivs n tri_tup plist ->
    match tri_tup with
    | (x,y) when x*y > ndivs -> n*(n+1)/2
    | (_,y) when n mod 2 = 0 -> trinext ndivs (n+1) (y,prime_fact (n/2+1) 1 1 plist) plist
    | (_,y) when n mod 2 = 1 -> trinext ndivs (n+1) (y,prime_fact (n+2) 1 1 plist) plist
    | _ -> trinext ndivs 2 (1,3) plist
  in let plist = eratosthenes 1000
  in trinext ndivs 0 (0,0) plist
