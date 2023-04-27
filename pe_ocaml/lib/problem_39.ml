open Core
let p39 () =
  let nmax = 1000
  in
  let rec gcd a b = match b with | 0 -> a | _ -> gcd b (a mod b)
  in
  let perimeters = Array.create ~len:nmax 0
  in
  let rec loop m n k =
    if (gcd m n) <> 1
    then (if n < (m-1) then loop m (n+1) 1 else loop (m+1) 1 1)
    else match (k*(m*m - n*n)+2*m*n*k+k*(m*m + n*n), n, k) with
      | _ when 2*m*(m+1) >= nmax -> perimeters
      | (abc,_,1) when abc >= nmax -> loop (m+1) 1 1
      | (abc,x,_) when abc >= nmax -> loop m (x+1) 1
      | (abc,x,y) -> (perimeters.(abc) <- perimeters.(abc) + 1; loop m x (y+1))
  in loop 2 1 1
   |> Array.foldi ~init:(0,0) ~f:(fun i (i0,n0) n -> if n > n0 then (i,n) else (i0,n0))
   |> fst
