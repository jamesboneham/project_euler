open Core

let p99 () =
  let base_exp = Stdio.In_channel.read_all "resources/p099_base_exp.txt"
                 (* |> String.substr_replace_all ~pattern:"\"" ~with_:"" *)
                 |> String.split_on_chars ~on:['\n']
                 |> List.(map ~f:(fun x -> String.split_on_chars ~on:[','] x
                                           |> map ~f:(fun y -> Int.of_string y |> float)))
  in List.(foldi base_exp ~init:(0, 2., 2.)
             ~f:(fun i (linum, a, b) x -> match x with
                 | [c; d] -> if Float.(a < c**(d/b))
                   then (i+1, c, d) else (linum, a, b)
                 | _ -> (0,0.,0.))) |> fst3
