let p4 = fun () ->
  let nmax = 999999
  in let nmin = nmax - (nmax mod (if nmax > 100_000 then 100_000 else 10_000))
  and pal_p = fun n ->
    let numstr = string_of_int n
    in let strlen = String.length numstr
    in numstr = (String.init strlen (fun i -> String.get numstr (strlen - 1 - i)))
  and pair_lookup = fun n ->
    let digits = [9;8;7;6;5;4;3;2;1]
    in List.concat_map
      (fun x ->
         List.filter_map (fun y -> if (x*y mod 10 = n) then Some (x,y) else None) digits)
      digits
  in let rec scan_pair = fun pmax (a, b) ->
      let ab = a*b in
      let rec scan = fun pmax (a1, b1) ->
        let ab1 = a1*b1 in
        if ab1 < (max nmin pmax) then pmax
        else scan (if (pal_p ab1) then ab1 else pmax) (a1, b1-10)
      in if ab < (max nmin pmax) then pmax
      else scan_pair (scan pmax (a, b)) (a-10, b-10)
  in let ab_list = (nmax / (if nmax > 100_000 then 100_000 else 10_000))
                   |> pair_lookup
                   |> (List.map (fun (a3, b3) ->
                       (min (990 + a3) (nmax/(100 + b3)))
                       |> (fun x -> (x, (min (10*((nmax/x)/10)) 990)+b3))))
  in List.fold_left scan_pair 0 ab_list

let%test "gives correct answer" = (p4 ()) = 906609
