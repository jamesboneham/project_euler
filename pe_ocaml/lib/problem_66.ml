open Core
let p66 () =
  let get_coeff_generator =
    let rec gcd a b = match b with | 0 -> a | _ -> gcd b (a mod b) in
    let step sqrtx x n d =
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
        | Some (i,_) -> List.(i+1,and_lst >>| fst3)
        | None -> stepper ((a1,n1,d1) :: and_lst,n1,d1)
      in stepper @@ and_init n
    in
    let coeff_gen n =
      let (nrep, chain) = nreps n in
      let (rep, head) = List.(split_n chain nrep
                              |> fun (x,y) -> Bigint.(rev x >>| of_int, rev y >>| of_int)) in
      let cache = ref (head@rep) in
      let rec next () = match !cache with
        | car::cdr -> (cache := cdr; car)
        | [] -> (cache := rep; next ())
      in next
    in coeff_gen
  in
  let find_sol d =
    let get_a = get_coeff_generator d
    and bd = Bigint.of_int d in
    let step (h1,k1,h2,k2) = let a = get_a () in
      let h0,k0 = Bigint.(a*h1+h2, a*k1+k2) in
      (h0,k0,h1,k1)
    in
    let rec loop (h1,k1,h2,k2) =
      match Bigint.(h1*h1 - bd*k1*k1) with
      | x when Bigint.((x = one) && (k1 <> zero)) -> h1
      | _ -> loop @@ step (h1,k1,h2,k2)
    in loop Bigint.(one,zero,zero,one)
  in
  let find_max_sol dlim =
    let sqrs = Int.Set.of_list
        List.(init (Float.(dlim |> of_int |> sqrt |> iround_down_exn)+1) ~f:(fun x -> x*x))
    in let issqr = Int.Set.mem sqrs in
    let rec loop solmax dmax d =
      if d > dlim then dmax
      else
        (if issqr d then loop solmax dmax (d+1)
         else match find_sol d with
           | x when Bigint.(x > solmax) -> loop x d (d+1)
           | _ -> loop solmax dmax (d+1))
    in loop Bigint.zero 0 2
  in find_max_sol 1000
