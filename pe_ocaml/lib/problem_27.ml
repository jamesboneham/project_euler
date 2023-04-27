open Core

let p27 () =
  let nmax = 1000
  in let blist = Utils.eratosthenes nmax
  and alist = (List.init nmax ~f:(fun x -> (1-nmax) + 2*x))
  in let pset = Set.of_list (module Int) blist
  in let pmax = Set.max_elt_exn pset
  in let ptest n =
       let ulim = (n |> float |> sqrt |> Float.to_int |> (+) 1)
       in match n with
       | _ when n <= pmax -> Set.mem pset n
       | _ -> List.fold_until blist ~init:true ~finish:(fun x -> Printf.printf "Out of primes"; x)
                ~f:(fun _ x -> match x > ulim with
                    | true -> Stop true
                    | _ when n mod x = 0 -> Stop false
                    | _ -> Continue true)
  in let get_nprimes b a =
       let rec nprimes = fun b a n ->
         match (n*(n + a) + b) with
         | x when x < 2 -> n
         | x when ptest x -> nprimes b a (n+1)
         | _ -> n
       in nprimes b a 1
  in let scan_a b =
       List.fold alist ~init:(0,0)
         ~f:(fun (nmax, amax) a -> match get_nprimes b a with
             | x when x > nmax -> (x,a)
             | _ -> (nmax, amax))
  in let scan_b () =
       List.fold blist ~init:(0,0,0)
         ~f:(fun (nmax, amax, bmax) b -> match scan_a b with
             | (n, a) when n > nmax -> (n, a, b)
             | _ -> (nmax, amax, bmax))
  in scan_b () |> (fun (_, a, b) -> a*b)
