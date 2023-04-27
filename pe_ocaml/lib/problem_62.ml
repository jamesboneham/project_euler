open Core
let p62 () =
  let nmax = 10000
  and ncubes = 5
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
  let to_hash n =
    n |> to_digs |> List.sort ~compare:(fun x y -> Int.compare y x) |> to_num
  in
  List.(Hashtbl.(let cubemap = of_alist_multi (module Int)
                     List.(init nmax ~f:(fun i -> (i*i*i |> to_hash, i))) in
                 filter_inplace cubemap ~f:(fun x -> List.(length x >= ncubes));
                 to_alist cubemap)
        >>| (fun (_,x) -> Option.value_exn (min_elt x ~compare:Int.compare))
        |> min_elt ~compare:Int.compare
        |> Option.value_exn
        |> fun x -> x*x*x)
