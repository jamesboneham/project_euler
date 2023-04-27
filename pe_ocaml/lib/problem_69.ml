open Core
let p69 () =
  let nmax = 1_000_000 in
  let plim = Float.((log (of_int nmax))/(log 2.) |> iround_up_exn)
  in
  let primes = Utils.eratosthenes plim
  in List.fold_until primes ~init:1 ~finish:(fun _ -> 0)
    ~f:(fun n p -> match n*p with
        | np when np > nmax -> Stop n
        | np -> Continue np)
