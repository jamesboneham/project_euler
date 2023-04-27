let p33 () =
  let rec gcd a b = match b with | 0 -> a | _ -> gcd b (a mod b)
  in
  let check d2 d1 d0 =
    if ((10*d1+d0)*d2 = (10*d0+d2)*d1) || ((10*d0+d1)*d2 = (10*d2+d0)*d1)
    then Some (d1, d2)
    else None
  in
  let rec scan frac d2 d1 d0 = match (d1>=d2, d0) with
    | (true, _) when d2 >= 9 -> frac
    | (true, _) -> scan frac (d2+1) 1 1
    | (false, 10) -> scan frac d2 (d1+1) 1
    | (false, x) -> (match check d2 d1 x with
        | Some y -> scan ((frac, y) |> (fun ((n,d),(ny, dy)) -> (n*ny, d*dy))) d2 d1 (x+1)
        | _ -> scan frac d2 d1 (x+1))
  in scan (1,1) 2 1 1
   |> (fun (a,b) -> b/(gcd a b))
