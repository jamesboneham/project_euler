open Core
let p80 () =
  let digitsum n =
    Big_int_Z.(sqrt_big_int @@
               mult_big_int (big_int_of_int n)
                 (power (big_int_of_int 10) 198)
               |> string_of_big_int)
    |> (fun s -> String.slice s 0 100)
    |> String.fold ~init:0
      ~f:(fun acc c -> c |> String.of_char |> Int.of_string |> (+) acc)
  in List.(range 0 100
           |> filter ~f:(fun n -> Float.(of_int n |> sqrt |> to_int)
                                  |> fun x -> x*x <> n)
           >>| digitsum
           |> reduce_exn ~f:(+))
