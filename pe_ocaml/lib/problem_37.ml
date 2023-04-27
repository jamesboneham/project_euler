open Core
let p37 () =
  let pset = Int.Set.of_list @@ Utils.eratosthenes 1000000
  in
  let ptest n = Int.Set.mem pset n
  in
  let rtrunc n =
    let rec loop m = match m with
      | 0 -> true
      | x -> if ptest x then loop (x/10) else false
    in loop (n/10)
  and digs = [1;2;3;5;7;9]
  in
  let step prms =
    List.(fold prms ~init:([],[]) ~f:(fun (out,acc) p ->
        let pwr10 = Float.(of_int p |> log10 |> iround_up_exn |> Int.pow 10)
        in
        fold digs ~init:(out,acc) ~f:(fun (out1,acc1) d ->
            let p1 = d*pwr10 + p in
            match (ptest p1) with
            | true -> ((if rtrunc p1 then p1::out1 else out1),
                       (if d <> 2 && d <> 5 then p1::acc1 else acc1))
            | _ -> (out1, acc1))))
  in
  let multistep nmax =
    let rec loop truncs prms i = match (List.length truncs) >= nmax with
      | true -> List.reduce_exn ~f:(+) truncs
      | false -> let (t1, p1) = step prms in
        loop (t1@truncs) p1 (i+1)
    in loop [] [3;7] 0
  in multistep 11
