open Core
let p60 () =
  let pmax = 10000
  in
  let primes = Utils.eratosthenes pmax
  in
  let pset = Int.Set.of_list primes
  in
  let ptest n =
    if n < pmax then Int.Set.mem pset n
    else let plim = Float.(of_int n |> sqrt |> iround_down_exn) in
      List.fold_until primes ~init:() ~finish:(fun _ -> true)
        ~f:(fun () p -> match n%p with
            | 0 -> Stop false
            | x when x > plim -> Stop true
            | _ -> Continue ())
  and conc a b =
    let rec loop x y =
      function pwr10 when y < pwr10 -> pwr10*x + y
             | pwr10 -> loop x y (pwr10*10)
    in loop a b 10
  in
  let ptest2 p1 p2 = ptest (conc p1 p2) && ptest (conc p2 p1)
  in
  let get_quints plist =
    let p = Array.of_list plist in
    let ilim, jlim, klim, llim, mlim = Array.(length p) |> (fun x -> (x-5,x-4,x-3,x-2,x-1)) in
    let rec loop i j k l m pair trip quad =
      if i >= ilim then 0
      else match (pair, trip, quad) with
        | (false,_,_) when j>=jlim -> pairfalse (i+1) (i+2) (i+3) (i+4) (i+5)
        | (false,_,_) -> pairfalse i (j+1) (j+2) (j+3) (j+4)
        | (_,false,_) when k>=klim -> pairfalse i (j+1) (j+2) (j+3) (j+4)
        | (_,false,_) -> tripfalse i j (k+1) (k+2) (k+3)
        | (_,_,false) when l>=llim -> tripfalse i j (k+1) (k+2) (k+3)
        | (_,_,false) -> quadfalse i j k (l+1) (l+2)
        | _ when m>=mlim -> quadfalse i j k (l+1) (l+2)
        | _ -> let a,b,c,d,e = (p.(i),p.(j),p.(k),p.(l),p.(m)) in
          if ((ptest2 a e)&&(ptest2 b e)&&(ptest2 c e)&&(ptest2 d e))
          then a+b+c+d+e
          else loop i j k l (m+1) true true true
    and pairfalse i j k l m =
      let a,b,c,d = (p.(i),p.(j),p.(k),p.(l)) in
      let pair, trip, quad = (ptest2 a b)
                             |> (fun x ->
                                 if not x then (false, false, false)
                                 else (((ptest2 a c)&&(ptest2 b c))
                                       |> (fun y -> if not y then (x,false,false)
                                            else (x, y, (ptest2 a d)&&(ptest2 b d)&&(ptest2 c d)))))
      in
      loop i j k l m pair trip quad
    and tripfalse i j k l m =
      let a,b,c,d = (p.(i),p.(j),p.(k),p.(l)) in
      let trip, quad = (((ptest2 a c)&&(ptest2 b c))
                        |> (fun y -> if not y then (false,false)
                             else (y, (ptest2 a d)&&(ptest2 b d)&&(ptest2 c d))))
      in
      loop i j k l m true trip quad
    and quadfalse i j k l m =
      let a,b,c,d = (p.(i),p.(j),p.(k),p.(l)) in
      let quad = (ptest2 a d)&&(ptest2 b d)&&(ptest2 c d) in
      loop i j k l m true true quad
    in loop 0 0 1 2 3 false false false
  in
  get_quints primes
