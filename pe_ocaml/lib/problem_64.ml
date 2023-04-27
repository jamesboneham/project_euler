open Core
let p64 () =
  let nmax = 10_000 in
  let rec gcd a b = match b with | 0 -> a | _ -> gcd b (a mod b) in
  let sqrs = List.(Float.(of_int nmax |> sqrt |> iround_down_exn) + 1
                   |> init ~f:(fun x -> x*x)
                   |> Int.Set.of_list)
  and step sqrtx x n d =
    let n1 = ((x-d*d) |> fun y -> y/(gcd y n)) in
    let a1 = Float.(iround_down_exn @@ (sqrtx - (of_int d))/(of_int n1)) in
    (a1, n1, -a1*n1 - d)
  and and_init n =
    let a0 = Float.(sqrt @@ of_int n |> iround_down_exn) in
    ([(a0, 1, ~-a0)], 1, ~-a0)
  in
  let nreps n =
    let stepfn = Float.(step (sqrt @@ of_int n) n) in
    let rec stepper (and_lst,n,d) = let (a1,n1,d1) = stepfn n d in
      match List.(findi and_lst ~f:(fun _ (a0,n0,d0) -> (a1=a0 && n1=n0 && d1=d0))) with
      | Some (i,_) -> i+1
      | None -> stepper ((a1,n1,d1) :: and_lst,n1,d1)
    in stepper @@ and_init n
  in
  let rec loop count n = if n > nmax then count
    else match (if Int.Set.mem sqrs n then 0 else (nreps n) % 2) with
      | 0 -> loop count (n+1)
      | _ -> loop (count+1) (n+1)
  in loop 0 1
