open Core
let p41 () =
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
  and perm_to_num perm =
    let rec loop num p = match p with
      | [] -> num
      | car::cdr -> loop (num*10 + car) cdr
    in loop 0 perm
  in
  let poss_prms =
    List.(all_perms 7 (range 1 8) >>| perm_to_num |> sort ~compare:(fun x y -> Int.compare y x))
  and primes = Utils.eratosthenes @@ Float.(of_int 7654321 |> sqrt |> iround_down_exn)
  in
  let ptest n = match List.find primes ~f:(fun p -> n mod p = 0) with
    | Some (_ : int) -> false
    | None -> true
  in
  List.find_exn poss_prms ~f:ptest
