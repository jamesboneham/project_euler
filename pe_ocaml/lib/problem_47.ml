open Core
let p47 () =
  let nmax = Float.(of_int 150000 |> sqrt |> iround_down_exn)
  in
  let primes = Utils.eratosthenes nmax
  in
  let pfac n =
    let rec loop m facs plst = match plst with
      | _ when m = 1 -> facs
      | p::cdr when m mod p = 0 -> (if facs < 4 then loop (loop1 (m/p) p) (facs+1) cdr else 0)
      | _::cdr -> loop m facs cdr
      | [] -> facs + 1
    and loop1 m p = match m mod p with
      | 0 -> loop1 (m/p) p
      | _ -> m
    in loop n 0 primes
  in
  let rec loop n streak = match pfac n with
    | 4 -> if streak >= 3 then n-3 else loop (n+1) (streak+1)
    | _ -> loop (n+1) 0
  in loop 2 0
