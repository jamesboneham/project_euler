let p2 = fun fmax ->
  let rec even_fib = fun (f3n1, f3n0, fibsum) ->
    match (f3n1 > fmax) with
    | true -> (f3n1, f3n0, fibsum)
    | false -> even_fib (f3n1*4 + f3n0, f3n1, fibsum+f3n1)
  in (fun (_, _, fsum) -> fsum) @@ even_fib (2, 0, 0)

let%test "basic test" = (p2 100) = 44
let%test "gives correct answer" = (p2 4000000) = 4613732
