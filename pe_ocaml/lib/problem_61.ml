open Core
let p61 () =
  let oct_stream = Stream.from (fun n -> match n*(3*n - 2) with
      | x when x >= 10000 -> None
      | x -> Some x)
  and hept_stream = Stream.from (fun n -> match (n*(5*n - 3))/2 with
      | x when x >= 10000 -> None
      | x -> Some x)
  and hex_stream = Stream.from (fun n -> match n*(2*n - 1) with
      | x when x >= 10000 -> None
      | x -> Some x)
  and pent_stream = Stream.from (fun n -> match (n*(3*n - 1))/2 with
      | x when x >= 10000 -> None
      | x -> Some x)
  and sqr_stream = Stream.from (fun n -> match n*n with
      | x when x >= 10000 -> None
      | x -> Some x)
  and tri_stream = Stream.from (fun n -> match (n*(n + 1))/2 with
      | x when x >= 10000 -> None
      | x -> Some x)
  and safenext stream =
    match Stream.peek stream with
    | Some _ -> Some (Stream.next stream)
    | None -> None
  and all_perms n lst =
    let perms = ref []
    and arr = Array.of_list lst
    in
    let rec generate k =
      let rec loop i = match i>=k-1 with
        | true -> ()
        | false -> let i0 = (if k mod 2 = 0 then i else 0) in
          let a0 = arr.(i0) and ak = arr.(k-1) in
          (arr.(i0) <- ak; arr.(k-1) <- a0; generate (k-1); loop (i+1))
      in
      if k=1 then perms := Array.(to_list arr) :: !perms
      else (generate (k-1); loop 0)
    in (generate n; !perms)
  in
  let to_list strm =
    let rec loop lst switch =
      match (safenext strm, switch) with
      | (Some x, true) -> loop (x::lst) true
      | (Some x, false) when x >= 1000 -> loop (x::lst) true
      | (Some _, false) -> loop lst false
      | (None, _) -> lst
    in loop [] false
  in
  let lstmap = Map.of_alist_exn (module Int) [(8, to_list oct_stream);
                                              (7, to_list hept_stream);
                                              (6, to_list hex_stream);
                                              (5, to_list pent_stream);
                                              (4, to_list sqr_stream);
                                              (3, to_list tri_stream)]
  in
  let getnums n = Map.find_exn lstmap n
  and compat_next nchain lst1 =
    let last2 = List.(hd_exn nchain) % 100 in
    List.filter_map lst1 ~f:(fun n -> if (n/100)=last2 then Some (n::nchain) else None)
  in
  let step nextlst chains =
    match nextlst with
    | [] -> List.concat_map chains ~f:(fun chn -> compat_next chn [List.last_exn chn])
    | _ -> List.concat_map chains ~f:(fun chn -> compat_next chn nextlst)
  and chains0 = List.(getnums 8 >>| (fun x -> [x]))
  in
  let orderings = all_perms 5 [7;6;5;4;3]
  and check_order order =
    List.(match (fold_until order ~init:chains0 ~finish:(step [])
                   ~f:(fun chains n -> match step (getnums n) chains with
                       | [] -> Stop []
                       | x -> Continue x))
                |> concat with
         | [] -> None
         | _::cdr -> Some (reduce_exn ~f:(+) cdr))
  in
  List.find_map_exn orderings ~f:check_order
