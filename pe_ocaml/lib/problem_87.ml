open Core
let p87 nmax =
  let eratosthenes = fun ?(rev = false) nmax ->
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
  in let pow4 = Float.((float nmax) - 2.**2. - 2.**3. |> fun x -> x ** (1./4.) |> to_int) |> eratosthenes |> List.map ~f:(fun x -> x*x*x*x)
  and pow3 = Float.((float nmax) - 2.**2. - 2.**4. |> fun x -> x ** (1./3.) |> to_int) |> eratosthenes |> List.map ~f:(fun x -> x*x*x)
  and pow2 = Float.((float nmax) - 2.**3. - 2.**4. |> sqrt |> to_int) |> eratosthenes |> List.map ~f:(fun x -> x*x)
  and mask = Array.create ~len:nmax true
  in let nsums2 lim2 =
       let psum = nmax - lim2
       in List.(fold_until pow2 ~init:0 ~finish:(fun x -> x)
                  ~f:(fun n pw2 -> match (pw2 > lim2) with
                      | true -> Stop n
                      | false -> let sum = psum + pw2
                        in Continue (if mask.(sum) then (mask.(sum) <- false; n+1) else n)))
  in let nsums3 lim3 =
       List.(fold_until pow3 ~init:0 ~finish:(fun x -> x)
               ~f:(fun n pw3 -> match (pw3 > lim3) with
                   | false -> Continue (n + (nsums2 (lim3-pw3)))
                   | true -> Stop n))
  in List.(fold pow4 ~init:0 ~f:(fun n pw4 -> n + (nsums3 (nmax - pw4))))
