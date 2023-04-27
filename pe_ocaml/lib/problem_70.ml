open Core
let p70 () =
  let primes = Utils.eratosthenes ~rev:true 10000
  and to_digs n =
    let rec loop m digs = match (m mod 10, m/10) with
      | (0,0) -> digs
      | (x,y) -> loop y (x::digs)
    in loop n []
  and listcomp lst1 lst2 =
    let rec loop l1 l2 = match (l1,l2) with
      | (car1::cdr1, car2::cdr2) -> (match Int.compare car1 car2 with
          | 0 -> loop cdr1 cdr2
          | x -> x)
      | _ -> 0
    in loop lst1 lst2
  in
  let isperm n1 n2 =
    match List.(listcomp (to_digs n1 |> sort ~compare:Int.compare)
                  (to_digs n2 |> sort ~compare:Int.compare)) with
    | 0 -> true
    | _ -> false
  in
  let ulim = 10_000_000
  in
  let sqrtlim = ulim |> Float.of_int |> sqrt |> Float.iround_down_exn
  in
  let rec split_primes pbig psmall = match psmall with
    | p::cdr when p > sqrtlim -> split_primes (p::pbig) cdr
    | _ -> (pbig, psmall)
  in
  let pbig, psmall = split_primes [] primes
  in
  let rec find_cand p1 plist phimax = match plist with
    | p2::cdr -> let n = p1*p2 and phi = (p1-1)*(p2-1) in
      (if n > ulim then find_cand p1 cdr phimax
       else (if isperm n phi then Some (n, phi)
             else find_cand p1 cdr phimax))
    | [] -> None
  in
  List.fold pbig ~init:(1000,1) ~f:(fun (nmax,phimax) p1 -> match find_cand p1 psmall phimax with
      | Some (n,phi) when (n*phimax) < (nmax*phi) -> (n,phi)
      | _ -> (nmax, phimax))
  |> fst
