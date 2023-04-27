open Core
let p76 () =
  let nmax = 100
  in let k_from_ij i j = i*(nmax) + (j-1)
  and sumarray = Array.create ~len:((nmax+1)*nmax) 1
  in let set i j = (match (i,j) with
      | (_,1) | (0,_) -> 1
      | (x,y) when x < y ->  sumarray.(k_from_ij x (y-1))
      | (x,y) -> sumarray.(k_from_ij x (y-1)) + sumarray.(k_from_ij (x-y) y))
                   |> (fun z -> sumarray.(k_from_ij i j) <- z)
  in let rec fill_loop i j = match (i,j) with
      | (100,99) -> set 100 99; sumarray.(k_from_ij 100 99)
      | (100,y) -> set 100 y; fill_loop 0 (y+1)
      | (x,y) -> set x y; fill_loop (x+1) y
  in fill_loop 1 2
