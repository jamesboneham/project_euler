open Core
let p68 () =
  let empty = Int.Set.empty in
  let combinations pool r =
    let n = List.length pool
    in
    let inds = Array.of_list List.(range 0 r)
    in
    let get_comb () = Array.fold inds ~init:empty ~f:(fun comb i ->
        Int.Set.add comb (List.nth_exn pool i))
    in
    let rec iloop combs =
      match List.(find (range ~stride:~-1 (r-1) ~-1) ~f:(fun x -> inds.(x) <> x + n - r)) with
      | None -> combs
      | Some i -> (inds.(i) <- inds.(i) + 1; jloop combs (i+1))
    and jloop combs j = match (j >= r) with
      | true -> iloop ((get_comb ()) :: combs)
      | _ -> (inds.(j) <- inds.(j-1) + 1; jloop combs (j+1))
    in iloop [get_comb ()]
  and all_perms n lst =
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
  and digs = List.range 1 11 in
  let dset = Int.Set.of_list digs
  and setsum s = Int.Set.fold s ~init:0 ~f:(+) in
  let inner_combs = combinations digs 5
                    |> List.sort ~compare:(fun x y -> Int.(compare (setsum x) (setsum y)))
  and check_comb_perm comb perm =
    let allowed = Int.Set.(diff dset comb |> to_list) |> List.sort ~compare:Int.compare
    and psums = (match perm with
        | f::g::h::i::j::[] -> [f+g; g+h; h+i; i+j; j+f]
        | _ -> []) |> List.(sort ~compare:(fun x y -> Int.compare y x) )
    in List.(zip_exn allowed psums
             |> (function (a,b) :: cdr -> Some (a+b, cdr) | _ -> None)
             |> (function None -> None
                        | Some (sum0, cdr) ->
                          fold_until cdr ~init:() ~finish:(fun _ -> Some sum0)
                            ~f:(fun () (x,y) -> match (x+y = sum0) with
                                | true -> Continue ()
                                | _ -> Stop None)))
  and ring2int ring =
    List.(concat_map ring ~f:(fun (a,b,c) -> [a;b;c])
          >>| Int.to_string
          |> concat_map ~f:String.to_list
          |> String.of_char_list
          |> fun s ->
          if (String.length s) = 16 then Some (Int.of_string s) else None)
  in
  let perm_to_ring psum perm =
    let ring = (match perm with
        | f::g::h::i::j::[] ->
          (match List.([f+g; g+h; h+i; i+j; j+f] >>| ((-) psum)) with
           | a::b::c::d::e::[] -> [(a,f,g);(b,g,h);(c,h,i);(d,i,j);(e,j,f)]
           | _ -> [])
        | _ -> [])
    in
    let min_outer = fst3 @@ Option.value_exn @@ List.min_elt ring
        ~compare:(fun (a1,_,_) (a2,_,_) -> Int.compare a1 a2)
    in
    let rec rotate_ring rng = match rng with
      | (a,f,g) :: cdr when a <> min_outer -> rotate_ring (cdr @ [(a,f,g)])
      | _ -> rng
    in rotate_ring ring |> ring2int
  in
  let check_comb comb =
    let comb_l = Int.Set.to_list comb in
    let perms = all_perms 5 comb_l in
    List.(filter_map perms ~f:(fun p -> match check_comb_perm comb p with
        | None -> None
        | Some x -> (match perm_to_ring x p with
            | None -> None
            | Some y -> Some y)))
  in
  List.(find_map inner_combs ~f:(fun c -> match check_comb c with
      | [] -> None
      | x -> max_elt ~compare:Int.compare x))
  |> Option.value_exn
