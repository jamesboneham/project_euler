open Core
let p67 () =
  let tri = Stdio.In_channel.read_all "resources/p067_triangle.txt"
            |> String.split_on_chars ~on:['\n']
            |> List.(map ~f:(String.(split_on_chars ~on:[' '])))
            |> List.rev
            |> List.tl_exn
            |> List.(map ~f:(fun s -> s >>| Int.of_string))
  and merge_rows row1 row2 =
    let rec loop r0 r1 r2 = match (r1, r2) with
      | (x1::y1::cdr1, x2::cdr2) -> loop Int.((max x1 y1)+x2 :: r0) (y1::cdr1) cdr2
      | (x1::[], x2::[]) -> loop (x1+x2 :: r0) [] []
      | _ -> List.(rev r0)
    in loop [] row1 row2
  in List.(reduce_exn tri ~f:merge_rows |> hd_exn)
