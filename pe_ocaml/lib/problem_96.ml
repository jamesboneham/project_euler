open Core

let p96 () =
  let puzzles = Stdio.In_channel.read_lines "resources/p096_sudoku.txt"
                |> List.groupi ~break:(fun i _ _ -> i mod 10 = 0)
                |> List.map ~f:(function _::b -> b | x -> x)
                |> List.map ~f:(List.concat_map
                                  ~f:(fun s -> String.to_list s
                                               |> List.map ~f:(fun c -> Int.of_string (String.of_char c))))
  and inds = List.range 0 81
  and is_solved puzzle = not (List.mem puzzle 0 ~equal:(=))
  and get_group = fun n -> match (n/9, n mod 9) with
    | (x, y) when x < 3 -> if y < 3 then 0 else (if y < 6 then 1 else 2)
    | (x, y) when x < 6 -> if y < 3 then 3 else (if y < 6 then 4 else 5)
    | (_, y) -> if y < 3 then 6 else (if y < 6 then 7 else 8)
  in let indmap = Int.Map.of_alist_exn
         (List.init (List.length inds)
            ~f:(fun n -> let (row, col, grp) = (n/9, n mod 9, get_group n) in
                 (n, List.(concat [filter inds ~f:(fun x -> (x/9) = row);
                                   filter inds ~f:(fun x -> (x mod 9) = col);
                                   filter inds ~f:(fun x -> (get_group x) = grp)])
                     |> Int.Set.of_list
                     |> Int.Set.to_list)))
  and digs = [1; 2; 3; 4; 5; 6; 7; 8; 9]
  in let allowed = fun i puzzle ->
      let forbidden = List.(map (Int.Map.find_exn indmap i)
                              ~f:(fun j -> nth_exn puzzle j)
                            |> filter ~f:(fun n -> n <> 0))
      in List.(filter digs ~f:(fun n -> not (mem forbidden n ~equal:(=))))
  and missingnums = fun puzzle ilist ->
    List.(map ilist ~f:(fun i -> nth_exn puzzle i)
          |> (fun l -> filter digs ~f:(fun n -> not (mem l n ~equal:(=)))))
  and missingcells = fun puzzle ilist ->
    List.(filter ilist ~f:(fun i -> (nth_exn puzzle i) = 0))
  and fillval puzzle ind value =
    List.mapi puzzle ~f:(fun i v -> if i=ind then value else v)
  in let strategy0 puzzle =
       let update_1 = fun i n puzzle -> match (n, allowed i puzzle) with
         | (0, x::[]) -> x
         | (y, _) -> y
       in let update puzzle = List.mapi puzzle ~f:(fun i n -> update_1 i n puzzle)
       in let rec solve puzzle = match (update puzzle) with
           | x when ((not (List.mem x 0 ~equal:(=))) || (List.equal (=) x puzzle)) -> x
           | x -> solve x
       in solve puzzle
  in let strategy1 puzzle =
       let select_group n =
         List.filter inds ~f:(fun i -> (get_group i) = n)
       and select_row n =
         List.filter inds ~f:(fun i -> (i / 9) = n)
       and select_col n =
         List.filter inds ~f:(fun i -> (i mod 9) = n)
       in let puz0 = strategy0 puzzle
       and crgfill selectfun puzzle =
         let findfun = fun ind ->
           let group = selectfun ind
           in let mnums = missingnums puzzle group
           and mcells = missingcells puzzle group
           in let allwdnums = List.map mcells ~f:(fun x -> (x, allowed x puzzle))
           in let fillnums = List.(
               filter mnums ~f:(fun y -> (count allwdnums ~f:(fun (_, x) -> mem x y ~equal:(=))) = 1))
           in List.(map fillnums
                      ~f:(fun n -> (find_exn allwdnums ~f:(fun (_, l) -> mem l n ~equal:(=)))
                                   |> (fun (i,_) -> (i, n))))
         in let newvals = List.(concat_map (range 0 9) ~f:(fun i -> findfun i))
         in List.fold newvals ~init:puzzle ~f:(fun puz (i,v) -> fillval puz i v)
       in let rec solve puzzle =
            match (puzzle
                   |> crgfill select_group |> crgfill select_col
                   |> crgfill select_row |> strategy0) with
            | x when ((not (List.mem x 0 ~equal:(=))) || (List.equal (=) x puzzle)) -> x
            | x -> solve x
       in if (List.mem puz0 0 ~equal:(=)) then solve puz0 else puz0
  in let strategy2 puzzle =
       let puz0 = strategy1 puzzle in
       let guess i =
         allowed i puz0
         |> List.fold_until ~init:puz0 ~finish:(fun x -> x)
           ~f:(fun puz v -> match strategy1 (fillval puz i v) with
               | p when is_solved p -> (Stop p)
               | _ -> Continue puz)
       and mcells = missingcells puz0 inds
       in List.fold_until mcells ~init:puz0 ~finish:(fun x -> x)
         ~f:(fun puz i -> match guess i with
             | p when is_solved p -> (Stop p)
             | _ -> Continue puz)
  in List.(puzzles >>| strategy2
           >>| (function (a::b::c::_) -> 100*a + 10*b + c | _ -> 0)
           |> reduce_exn ~f:(+))
