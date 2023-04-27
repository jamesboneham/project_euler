open Core
let p49 () =
  let primes = Utils.eratosthenes 9999 |> List.filter ~f:((<) 1000)
  and to_digs p =
    let rec loop digs n = match (n mod 10, n/10) with
      | (0,0) -> digs
      | (x,y) -> loop (x::digs) y
    in List.sort (loop [] p) ~compare:Int.compare
  and listcomp lst1 lst2 =
    let rec loop l1 l2 = match (l1,l2) with
      | (car1::cdr1, car2::cdr2) -> (match Int.compare car2 car1 with
          | 0 -> loop cdr1 cdr2
          | x -> x)
      | _ -> 0
    in loop lst1 lst2
  and check_perms perms =
    let ilim = (List.length perms) - 3
    in let jlim = ilim + 1
    in
    let rec loop i j = match (i>ilim, j>jlim) with
      | (true,_) -> None
      | (_,true) -> loop (i+1) (i+2)
      | _ -> let (pi,pj) = List.(nth_exn perms i, nth_exn perms j) in
        (match List.find perms ~f:((=) (2*pj-pi)) with
         | Some _ when pi=1487 -> None
         | Some pk -> Some (pi*100000000+pj*10000+pk)
         | None -> loop i (j+1))
    in loop 0 1
  in
  List.(primes >>| (fun p -> (p, to_digs p))
        |> sort ~compare:(fun (_,x) (_,y) -> listcomp x y)
        |> group ~break:(fun (_,x) (_,y) -> listcomp x y <> 0)
        |> filter_map ~f:(fun x -> if (length x) >= 3 then Some (x >>| fst) else None)
        |> filter_map ~f:check_perms
        |> hd_exn)
