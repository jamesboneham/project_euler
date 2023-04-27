open Core
let p46 () =
  let nmax = 6000
  in
  let primes = Int.Set.of_list @@ Utils.eratosthenes nmax
  and sqrs2 = List.init Float.(of_int Int.(nmax/2) |> sqrt |> iround_down_exn)
      ~f:(fun x -> 2*Int.(pow (x+1) 2))
  in
  let test_n n =
    List.fold_until sqrs2 ~init:() ~finish:(fun () -> false) ~f:(fun () s ->
        if s >= n then Stop false else
          match Int.Set.mem primes (n-s) with
          | true -> Stop true
          | _ -> Continue ())
  in
  let find_exception () =
    let rec loop n = match (Int.Set.mem primes n || test_n n) with
      | _ when n >= nmax -> 0
      | true -> loop (n+2)
      | false -> n
    in loop 9
  in
  find_exception ()
