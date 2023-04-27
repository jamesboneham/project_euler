open Core

let p92 = fun nmax ->
  let rec digsum acc n = match n with
    | 0 -> acc
    | x -> digsum (acc + (Int.pow (x mod 10) 2)) (x/10)
  in let sievearr = Array.create ~len:nmax (false, false)
  in sievearr.(1) <- (true, false); sievearr.(89) <- (true, true);
  let rec sieve n89 chain n = match sievearr.(n) with
    | (false, (_ : bool)) -> sieve n89 (n :: chain) (digsum 0 n)
    | (true, x) -> List.fold chain ~init:n89
                     ~f:(fun acc m -> sievearr.(m) <- (true, x); (if x then acc + 1 else acc))
  in let rec scan n89 n = match n >= nmax with
      | true -> n89
      | false -> scan (sieve n89 [] n) (n+1)
  in scan 1 1
