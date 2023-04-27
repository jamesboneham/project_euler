open Core

let p24 = fun nperm ->
  let factorial = fun n ->
    let rec innerfact fact m = match m with
      | x when x < 0 -> 0
      | 0 -> fact
      | _ -> innerfact (m*fact) (m-1)
    in innerfact 1 n
  and nums = List.range 0 10
  and digit_pop i lst =
    List.(nth_exn lst i |> (fun x -> (x, filter ~f:(fun y -> y <> x) lst)))
  in let get_indices = fun nperm ->
      let rec get_indices_inner n d indices =
        match (d, factorial d) with
        | (1,x) -> List.(rev (0::(n/x)::indices))
        | (_,x) -> get_indices_inner (n mod x) (d-1) ((n/x)::indices)
      in get_indices_inner (nperm-1) 10 [] |> List.tl_exn
  and get_str = fun indices ->
    let rec get_str_inner = fun s inds digs ->
      match inds with
      | [] -> s
      | car::cdr ->
        let (d,dlist) = digit_pop car digs
        in get_str_inner (s^(Int.to_string d)) cdr dlist
    in get_str_inner "" indices nums
  in get_str (get_indices nperm)
