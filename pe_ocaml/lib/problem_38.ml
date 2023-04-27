open Core
let p38 () =
  let all_perms n lst =
    let perms = ref []
    and arr = Array.of_list lst
    in
    let rec generate k =
      let rec loop i = match i>=k-1 with
        | true -> ()
        | false -> let i0 = (if k mod 2 = 0 then i else 0) in
          let a0 = arr.(i0) and ak = arr.(k-1) in
          (arr.(i0) <- ak; arr.(k-1) <- a0; generate (k-1); loop (i+1))
      in
      if k=1 then perms := Array.(to_list arr) :: !perms
      else (generate (k-1); loop 0)
    in (generate n; !perms)
  and sortfun perm1 perm2 =
    let rec loop p1 p2 = match p1,p2 with
      | (car1::cdr1, car2::cdr2) -> (match Int.compare car1 car2 with
          | 0 -> loop cdr1 cdr2
          | x -> ~-x)
      | (_, _) -> 0
    in loop perm1 perm2
  and check_perm perm = match perm with
    | d1::d2::d3::d4::d5::d0::[] when (200*d0+20*d1+2*d2) = (100*d3+10*d4+1*d5) ->
      Some (900018000 + (100*d0+10*d1+1*d2)*100000 + (100*d3+10*d4+1*d5))
    | _ -> None
  in
  let find_num ini =
    List.(fold_until (all_perms 5 ini |> sort ~compare:sortfun) ~init:() ~finish:(fun _ -> 0)
            ~f:(fun () perm -> match check_perm perm with
                | Some x -> Stop x
                | None -> Continue ()))
  in
  match find_num [7;6;5;3;2;4] with
  | 0 -> (match find_num [7;6;5;4;2;3] with
      | 0 -> find_num [7;6;5;4;2;3]
      | y -> y)
  | x -> x
