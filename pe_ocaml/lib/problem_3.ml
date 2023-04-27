let p3 = fun n ->
  let rec hpf = fun f n_rem fmax ->
    match (f > n_rem) with
    | true -> fmax
    | false -> (if (n_rem mod f = 0)
                then (hpf f (n_rem/f) f)
                else (hpf (f + (if (f=2) then 1 else 2)) n_rem fmax))
  in hpf 2 n 1

let%test "basic test" = (p3 13195) = 29
let%test "gives correct answer" = (p3 600851475143) = 6857
