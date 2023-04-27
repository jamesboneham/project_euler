open Core
let p43 () =
  let all_digs = List.(range 0 10)
  and to_digs n =
    let rec loop m d = match (m mod 10, m/10) with
      | (0,0) -> d
      | (x,y) -> loop y (x::d)
    in loop n []
  and to_num digs =
    List.fold digs ~init:0 ~f:(fun n d -> 10*n + d)
  in
  let poss17 =
    List.(range 6 59
          >>| (( * ) 17)
          >>| to_digs
          |> filter ~f:(fun x -> not (contains_dup x ~compare:Int.compare)))
  and next_num quot stem =
    let n0 = match stem with
      | a::b::_ -> 10*a + b
      | _ -> 0
    and digs = List.(filter all_digs ~f:(fun d -> not (mem stem d ~equal:(=))))
    in
    List.(fold digs ~init:[] ~f:(fun acc d -> match 100*d + n0 with
        | x when x mod quot = 0 -> (d::stem) :: acc
        | _ -> acc))
  in
  List.(fold [13;11;7;5;3;2;1] ~init:poss17
          ~f:(fun nums quot -> concat_map nums ~f:(next_num quot))
        >>| to_num
        |> reduce_exn ~f:(+))
