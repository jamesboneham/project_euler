open Core

module IntTuple = struct
  module T = struct
    type t = int * int
    let compare (x0,x1) (y0,y1) = match Int.compare x0 y0 with
      | 0 -> Int.compare x1 y1
      | z -> z
    let hash = Hashtbl.hash
    let sexp_of_t (x,y) : Sexp.t = List [ Atom (Int.to_string x); Atom (Int.to_string y)]
  end
  include T
  include Comparator.Make(T)
end

let p53 () =
  let index =
    let rec loop nr_tup n r i = match (n,r) with
      | (101,_) -> nr_tup
      | (m,q) when q > m -> loop nr_tup (m+1) 1 i
      | (m,q) -> loop (((n,r),i) :: nr_tup) m (q+1) (i+1)
    in Core.Map.of_alist_exn (module IntTuple) (loop [] 1 1 0)
  in
  let ind n r = Core.Map.find_exn index (n,r)
  in
  let coeff_arr = Array.create ~len:((ind 100 100)+1) 0
  in
  let rec get_nr n r =
    if r = n || r = 0 then 1
    else let i = ind n r in
      match coeff_arr.(i) with
      | 0 -> let x = min ((get_nr (n-1) (r-1)) + (get_nr (n-1) r)) 1_000_000 in
        (coeff_arr.(i) <- x; x)
      | x -> x
  in
  ((List.(range 1 101 >>| get_nr 100) ): int list) |> ignore;
  Array.count coeff_arr ~f:((=) 1_000_000)
