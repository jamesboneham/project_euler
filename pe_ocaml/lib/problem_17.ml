open Core
let p17 = fun nmax ->
    let rec getdlist = fun n dlist ->
    match dlist with
    | [] -> getdlist (n mod 1000) [n/1000]
    | [a] -> getdlist (n mod 100) [a;n/100]
    | [a;b] -> getdlist (n mod 10) [a;b;n/10]
    | [a;b;c] -> [a;b;c;n]
    | _ -> []
  in let getdig = fun n ->
    match n with
    | 1 | 2 | 6 -> 3
    | 4 | 5 | 9 -> 4
    | 3 | 7 | 8 -> 5
    | _ -> 0
  in let get10 = fun n10 n1 ->
      (getdig n1) + match n10 with
      | 1 -> (match n1 with
          | 1 | 2 | 3 | 5 | 8 | 0 -> 3
          | 7 | 4 | 6 | 9 -> 4
          | _ -> -10000)
      | 4 | 5 | 6 -> 5
      | 2 | 3 | 8 | 9 -> 6
      | 7 -> 7
      | _ -> 0
  in let getlen = fun dlist ->
      match List.([getdig (nth_exn dlist 0); getdig (nth_exn dlist 1);
                   get10 (nth_exn dlist 2) (nth_exn dlist 3)]) with
      | [a;b;c] -> (if a <> 0 then a + 8 else 0)
                   + (if b <> 0 then b + 7 else 0)
                   + (if c <> 0 && (b <> 0 || a <> 0) then c + 3 else c)
      | _ -> 0
  in List.(fold_left (range 1 (nmax+1)) ~init:0
             ~f:(fun acc num -> acc + (getlen (getdlist num []))))
