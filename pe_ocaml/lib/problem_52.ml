open Core
let p52 () =
  let listcomp lst1 lst2 =
    let rec loop l1 l2 = match (l1,l2) with
      | (car1::cdr1, car2::cdr2) -> (match Int.compare car2 car1 with
          | 0 -> loop cdr1 cdr2
          | x -> x)
      | _ -> 0
    in loop lst1 lst2
  and to_digs n =
    let rec loop m digs = match (m mod 10, m/10) with
      | (0,0) -> digs
      | (x,y) -> loop y (x::digs)
    in loop n [] |> List.sort ~compare:Int.compare
  in
  let check_num num =
    let digs = to_digs num in
    let rec loop x0 x n = match listcomp digs @@ to_digs x with
      | 0 -> if n=6 then Some x0 else (loop x0 (x+x0) (n+1))
      | _ -> None
    in loop num (num+num) 2
  in
  let rec loop n = match check_num n with
    | None -> loop (n+2)
    | Some x -> x
  in
  loop 100001
