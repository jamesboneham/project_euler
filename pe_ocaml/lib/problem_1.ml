let p1 nmax =
  let nsum a = let n = (nmax-1)/a
    in a*n*(n+1)/2
  in (nsum 3) + (nsum 5) - (nsum 15)

let%test "basic test" = (p1 10) = 23
let%test "gives correct answer" = (p1 1000) = 233168
