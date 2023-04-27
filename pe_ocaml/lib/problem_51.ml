open Core
let p51 () =
  let primes = Utils.eratosthenes 1000000
  in
  let pset = Int.Set.of_list primes
  and to_digs n =
    let rec loop m digs = match (m mod 10, m/10) with
      | (0,0) -> digs
      | (x,y) -> loop y (x::digs)
    in loop n []
  and to_num digs =
    let rec loop d n = match d with
      | [] -> n
      | car::cdr -> loop cdr (10*n + car)
    in loop digs 0
  in
  let all_digs = List.(range 0 10)
  in
  let multi_dig digs d =
    let build_nums mask =
      List.(all_digs >>| (fun d1 ->
          map2_exn digs mask ~f:(fun d0 m -> if m then d1 else d0))
            |> filter_map ~f:(fun x -> match x with
                | 0::_ -> None
                | y -> Some (to_num y)))
    and count_primes nlist =
      match List.count nlist ~f:(fun n -> Int.Set.mem pset n) with
      | x when x >= 8 -> Some List.(hd_exn nlist)
      | _ -> None
    in
    let masks =
      match List.count digs ~f:((=) d) with
      | 3 -> Some [List.map digs ~f:(fun x -> x=d)]
      | 4 -> let x = List.map digs ~f:(fun x -> x=d) in
        Some List.(filter_mapi x ~f:(fun i x -> if x then Some i else None)
                   >>| (fun i -> mapi x ~f:(fun j y -> if j=i then false else y)))
      | _ -> None
    in
    let nums =
      match masks with
      | Some msks -> Some List.(msks >>| build_nums |> filter_map ~f:count_primes)
      | None -> None
    in match nums with
    | Some (x::_) -> Some x
    | _ -> None
  in
  List.(fold_until primes ~init:() ~finish:(fun _ -> 0) ~f:(fun () p ->
      match filter_map [0;1;2] ~f:(fun d -> multi_dig (to_digs p) d) with
      | x::_ -> Stop x
      | _ -> Continue ()))
