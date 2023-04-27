open Core
let p40 () =
  let choose_dig (n,i) =
    String.nget (Int.to_string n) i |> Char.to_string |> Int.of_string
  in
  let get_nth_dig n =
    let rec loop i d j step nextinc = match (j = step, i=nextinc) with
      | _ when d >= n -> (i, j-1)
      | (_,true) -> loop i (d+1) 2 (step+1) (nextinc*10)
      | (true,_) -> loop (i+1) (d+1) 1 step nextinc
      | _ -> loop i (d+1) (j+1) step nextinc
    in loop 1 1 1 1 10
  in
  List.fold ~init:1 ~f:(fun acc n -> get_nth_dig n |> choose_dig |> ( * ) acc)
    [1;10;100;1000;10000;100000;1000000]
