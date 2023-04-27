open Core
module IntTuple = struct
    type t = int * int
    let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let hash = Hashtbl.hash
  end

let p77 nmax =
  let primes = Utils.eratosthenes 74
  in let (pprev,pnext) = List.(
      primes
      |> (fun p -> map2_exn (tl_exn p) (drop_last_exn p)
             ~f:(fun x y -> ((x,y),(y,x))))
      |> (fun z -> (z >>| fst |> Int.Map.of_alist_exn,
                    z >>| snd |> Int.Map.of_alist_exn))
      |> (fun (m1,m2)
           -> ((fun p -> match Int.Map.find m1 p with
               | Some p1 -> p1
               | None -> 0),
               (fun p -> match Int.Map.find m2 p with
                  | Some p1 -> p1
                  | None -> 0))))
  and table = Hashtbl.of_alist_exn (module IntTuple) [((2,2), 1)]
  in  let find_oob (n,p) = match (Hashtbl.find table (n,p), n) with
      | (_, 0) -> 1
      | (_, 1) -> 0
      | (Some x, _) -> x
      | (None, _) -> raise (Not_found_s (Sexp.of_string (Printf.sprintf "find_oob: not found (%d,%d)" n p)))
  in let getnways = function (n,p) when (n<3 || p=2) -> (1 - (n mod 2))
                           | (n,p) when (p>n) -> find_oob (n,pprev p)
                           | (n,p) when (p=n) -> ((find_oob (n,pprev p)) + 1)
                           | (n,p) -> ((find_oob (n,pprev p)) + (find_oob (n-p,p)))
  in let rec add_row n1 p0 pj =
       match pj with
       | 2 -> Hashtbl.add_exn ~key:(n1,2) ~data:(getnways (n1,2)) table; add_row n1 p0 3
       | p when (p > p0) -> (n1, p0)
       | p -> Hashtbl.add_exn ~key:(n1,p) ~data:(getnways (n1,p)) table; add_row n1 p0 (pnext p)
  and add_col n0 p1 ni =
    match ni with
    | 2 -> Hashtbl.add_exn ~key:(2,p1) ~data:(getnways (2,p1)) table; add_col n0 p1 3
    | n when (n > n0) -> (n0, p1)
    | n -> Hashtbl.add_exn ~key:(n,p1) ~data:(getnways (n,p1)) table; add_col n0 p1 (n+1)
  in let rec build_table (n,p) =
       match (n, p) with
       | (n,p) when (n >= p) -> build_table @@ add_col n (pnext p) 2
       | _ when getnways (n,p) >= nmax -> n
       | (n,p) -> build_table @@ add_row (n+1) p 2
  in build_table (2,2)
