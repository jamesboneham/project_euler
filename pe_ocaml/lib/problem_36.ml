open Core
let p36 () =
  let to_bin n =
    let rec loop m bin = match (m, Int.ctz m) with
      | (0,_) -> bin
      | (_,0) -> loop (m lsr 1) ("1"^bin)
      | (_,x) -> loop (m lsr x) ((String.init x ~f:(fun _ -> '0'))^bin)
    in loop n ""
  in
  let is_bin_pal n =
    let nbin = to_bin n
    in String.(nbin = (rev nbin))
  and get_pals () =
    let rec loop0 pals d0_e d0_o = match d0_o with
      | 11 -> pals
      | x -> loop0 (d0_o::d0_e::pals) (d0_e + 22) (x + 2)
    and loop1 pals d0_e d0_o d1_e d1_o = match (d0_o,d1_o) with
      | (1111, _) -> pals
      | (x, 100) -> loop1 pals (d0_e + 2002) (x + 202) 0 0
      | (x, y) -> loop1 ((d0_e + d1_e) :: (d0_o + d1_o) :: pals) d0_e x (d1_e + 110) (y + 10)
    and loop2 pals d0_e d0_o d1_e d1_o d2_e d2_o = match (d0_o,d1_o,d2_o) with
      | (x, _, _) when x >= 100000 -> pals
      | (x, y, _) when y >= 10000 -> loop2 pals (d0_e + 200002) (x + 20002) 0 0 0 0
      | (x, y, z) when z >= 1000 -> loop2 pals d0_e x (d1_e + 10010) (y + 1010) 0 0
      | (x, y, z) -> loop2 ((d0_e + d1_e + d2_e) :: (d0_o + d1_o + d2_o) :: pals)
                       d0_e x d1_e y (d2_e + 1100) (z + 100)
    in List.concat [loop0 [] 11 1; loop1 [] 1001 101 0 0; loop2 [] 100001 10001 0 0 0 0]
  in List.(fold ~init:0 ~f:(fun acc p -> if is_bin_pal p then acc + p else acc)
           @@ get_pals ())
