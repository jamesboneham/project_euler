open Core
let p34 () =
  let facts =
    List.(range 0 10 >>|
          (fun x ->
             let rec fact f n =
               match n with | 0 -> f | y -> fact (y*f) (y-1)
             in (x, fact 1 x)))
    |> Int.Map.of_alist_exn
  in
  let get_fact n = Int.Map.find_exn facts n
  in
  let inc_facsum arr facsum =
    Array.fold_until arr ~init:(0,facsum) ~finish:(fun _ -> 0)
      ~f:(fun (i,sum) n -> match n with
          | 9 when Option.is_none (Array.findi arr ~f:(fun j n -> j > i && n <> 0)) ->
            Stop (arr.(i) <- 0; arr.(i+1) <- 1; sum - 362878)
          | 9 -> Continue (arr.(i) <- 0; (i+1, sum - 362879))
          | d -> Stop (arr.(i) <- (d+1); sum + (d)*(get_fact d)))
  in
  let find_facs nmax =
    let arr = Array.create ~len:(Float.(of_int nmax |> log10 |> iround_up_exn)+1) 0
    in
    arr.(0) <- 3;
    let rec loop facs n facsum = match n >= nmax with
      | true -> facs
      | false -> loop (if n = facsum then n+facs else facs) (n+1) (inc_facsum arr facsum)
    in loop 0 3 6
  in
  find_facs 50000
