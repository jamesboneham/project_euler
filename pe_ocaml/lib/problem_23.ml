open Core
open Utils

let p23 = fun nmax ->
  let get_anums = fun nmax ->
    let imax = nmax - 1
    and narray = Array.create ~len:nmax false
    and getsig = get_sigma_finder (nmax*2)
    in let abundant = fun n -> if (getsig n) - n > n then true else false
    in let rec set_mults = fun i0 i ->
        match i > imax with
        | true -> ()
        | false -> narray.(i) <- true; set_mults i0 (i+i0)
    and check_abund = fun i ->
      match (i+1,narray.(i)) with
      | (j,_) when j > imax -> ()
      | (j,n) when n -> check_abund j
      | (j,_) when abundant i -> set_mults i i; check_abund j
      | (j,_) -> check_abund j
    in check_abund 1;
    narray |> Array.filter_mapi ~f:(fun i n -> if n then Some i else None)
    |> Array.to_list
  in let getsums = fun nmax ->
      let anums = get_anums nmax
      in let rec add_sums = fun sums lst ->
          match lst with
          | car::(cadr::_) when car + cadr > nmax -> sums
          | car::cdr ->
            add_sums (List.fold_until lst ~init:sums ~finish:(fun x -> x)
                        ~f:(fun s n -> match (n+car) with
                            | x when x < nmax -> Continue (Set.add s x)
                            | _ -> Stop s)) cdr
          | _ -> sums
      in add_sums (Set.empty (module Int)) anums
  in let getnosums = fun nmax ->
      let sums = getsums nmax
      and full = Set.of_list (module Int) (List.range 1 nmax)
      in Set.diff full sums |> Set.fold ~init:0 ~f:(+)
  in getnosums nmax
