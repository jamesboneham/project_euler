open Core
let p50 () =
  let nmax = 1000000 in
  let primes = Utils.eratosthenes nmax in
  let prime_sums = Array.of_list
      (List.folding_map primes ~init:0 ~f:(fun acc x -> let y = acc+x in (y,y)))
  and pset = Int.Set.of_list primes
  in
  let find_chain arr =
    let imax = Array.length arr in
    let rec loop cwidth i j = match i>= imax with
      | true -> loop (cwidth-1) (cwidth-1) 0
      | false -> let p1 = (arr.(i) - arr.(j)) in
        if p1 > nmax then loop (cwidth-1) (cwidth-1) 0
        else (if Int.Set.mem pset p1 then p1
              else loop cwidth (i+1) (j+1))
    in loop (imax-1) (imax-1) 0
  in
  find_chain prime_sums
