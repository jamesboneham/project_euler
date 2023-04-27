open Core
let p58 () =
  let pmax = 30000 in
  let primes = Utils.eratosthenes pmax in
  let pset = Int.Set.of_list primes in
  let ptest n =
    if n < pmax then Int.Set.mem pset n
    else let plim = Float.(of_int n |> sqrt |> iround_down_exn) in
      List.fold_until primes ~init:() ~finish:(fun _ -> true)
        ~f:(fun () p -> match n%p with
            | 0 -> Stop false
            | x when x > plim -> Stop true
            | _ -> Continue ())
  in
  let rec loop n ntot nprimes step i = match i with
    | 4 when 10*nprimes < ntot -> step+1
    | 4 -> loop (n+2) ntot nprimes (step+2) 0
    | j -> loop (n+step) (ntot+1) (nprimes+(if ptest n then 1 else 0)) step (j+1)
  in
  loop 3 1 0 2 0
