open Core

let p31 () =
  let rec ncombs (n,c) = match (n,c) with
    | (0,_) | (_,1) -> 1
    | (_,2) -> n/2 + 1
    | _ -> List.fold ~f:(+) ~init:0
             (List.filter_map [1;2;5;10;20;50;100;200]
                ~f:(fun x -> (if (x <= c) && (x <= n)
                              then Some (ncombs (n-x, x))
                              else None)))
  in ncombs(200,200)
