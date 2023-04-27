let p6 = fun nmax ->
  let rec ssd = fun (n, nsum, nnsum) ->
    match (n > 0) with
    | true -> ssd (n-1, nsum+n, nnsum+n*n)
    | false -> (0, nsum*nsum, nnsum)
  in (fun (_,a,b) -> a-b) @@ ssd (nmax, 0, 0)

let%test "basic test" = (p6 10) = 2640
let%test "gives correct answer" = (p6 100) = 25164150
