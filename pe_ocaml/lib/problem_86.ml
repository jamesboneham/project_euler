open Core
(* Horrible, needs optimizing takes ~6s to run AFTER compiling (i.e. don't try running it in utop!) *)
let p86 nmax =
  let square_p n = n |> float |> sqrt |> int_of_float |> fun x -> x*x |> (=) n
  in let rec countroutes (a,b,m) l n =
    let n1 = if (square_p l) then n + 1 else n
    in match true with
    | true when (a >= m) -> n1
    | true when (b >= m) -> countroutes (a+1,a+1,m) (4*(a+1)*(a+1) + m*m) n1
    | (_ : bool) -> countroutes (a,b+1,m) (l + 2*(b+a) + 1) n1
  and mloop m n =
    let n1 = countroutes (1,1,m) (m*m + 4) n
    in match (n1 > nmax) with
    | true -> m
    | false -> mloop (m+1) n1
  in mloop 1 0
