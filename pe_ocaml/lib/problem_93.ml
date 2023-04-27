open Core

let p93 () =
  let nmax = 9
  and ( +:: ) = fun x y -> match (x, y) with
    | (Some a, Some b) -> Some (a+b)
    | _ -> None
  and ( -:: ) = fun x y -> match (x, y) with
    | (Some a, Some b) -> if a-b > 0 then Some (a-b) else Some (b-a)
    | _ -> None
  and ( /:: ) = fun x y -> match (x, y) with
    | (Some 0, _) | (_, Some 0) -> Some 0
    | (Some a, Some b) -> if a mod b = 0 then Some (a/b) else (if b mod a = 0 then Some (b/a) else None)
    | _ -> None
  and ( *:: ) = fun x y -> match (x, y) with
    | (Some a, Some b) -> Some (a*b)
    | _ -> None
  in let ops = [( +:: ); ( -:: ); ( /:: ); ( *:: )]
  and inds = List.(range 0 (4*4*4))
  in let rec get_oplist l i = match l with
      | [a;b;c] -> [a;b;c]
      | x -> get_oplist ((List.nth_exn ops (i mod 4)) :: x) ((i - (i mod 4)) / 4)
  in let getpermlist = fun l i ->
      match (l, get_oplist [] i) with
      | ([a;b;c;d], [(+!); (+!!); (+!!!)]) ->
        [(Some a) +! (Some b) +!! (Some c) +!!! (Some d);
         (Some a) +! ((Some b) +!! (Some c) +!!! (Some d));
         ((Some a) +! (Some b) +!! (Some c)) +!!! (Some d);
         ((Some a) +! (Some b)) +!! (Some c) +!!! (Some d);
         (Some a) +! (Some b) +!! ((Some c) +!!! (Some d));
         (Some a) +! ((Some b) +!! (Some c)) +!!! (Some d);
         ((Some a) +! (Some b)) +!! ((Some c) +!!! (Some d));]
        |> List.filter_opt |> List.sort ~compare:Int.compare
        |> List.remove_consecutive_duplicates ~equal:(=)
      | _ -> []
  in let getfullperm = fun l ->
      List.(inds |> concat_map ~f:(fun i -> getpermlist l i)
            |> List.sort ~compare:Int.compare
            |> List.remove_consecutive_duplicates ~equal:(=)
            |> fun l1 -> match l1 with
            | 0::cdr -> cdr
            | _ -> l1)
  in let getperms = function [a;b;c;d] ->
      let nextperm l =
        List.(fold [a;b;c;d] ~init:[] ~f:(fun acc n -> if mem ~equal:(=) l n then acc else (n :: l) :: acc))
      in List.([[a]; [b]; [c]; [d]]
               |> concat_map ~f:nextperm
               |> concat_map ~f:nextperm
               |> concat_map ~f:nextperm)
                           | _ -> []
  in let getchainlength a b c d =
       List.(concat_map (getperms [a;b;c;d]) ~f:getfullperm
             |> sort  ~compare:Int.compare
             |> remove_consecutive_duplicates ~equal:(=)
             |> group ~break:(fun x y -> (x+1) <> y)
             |> hd_exn
             |> (fun l -> match l with
                 | 1::_ -> length l
                 | _ -> 0))
  in let rec loop (comb, maxchain) a b c d = match (a,b,c,d, getchainlength a b c d) with
      | (w,x,y,z,l) when w >= nmax - 3 -> if l >= maxchain then ((w,x,y,z),l) else (comb, maxchain)
      | (w,x,y,z,l) when x >= nmax - 2 -> loop (if l >= maxchain then ((w,x,y,z),l) else (comb, maxchain))
                                            (w+1) (w+2) (w+3) (w+4)
      | (w,x,y,z,l) when y >= nmax - 1 -> loop (if l >= maxchain then ((w,x,y,z),l) else (comb, maxchain))
                                            w (x+1) (x+2) (x+3)
      | (w,x,y,z,l) when z >= nmax -> loop (if l >= maxchain then ((w,x,y,z),l) else (comb, maxchain))
                                        w x (y+1) (y+2)
      | (w,x,y,z,l) -> loop (if l >= maxchain then ((w,x,y,z),l) else (comb, maxchain)) w x y (z+1)
  in loop ((0,0,0,0), 0) 1 2 3 4
     |> fun ((a,b,c,d), _) -> Int.((to_string a) ^ (to_string b) ^ (to_string c) ^ (to_string d) |> of_string)
