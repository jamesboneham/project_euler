open Core
let p71 () =
  List.(range 999994 1000001
        |> filter ~f:(fun x -> x%7 <> 0)
        |> max_elt ~compare:(fun d1 d2 -> Int.(compare (((3*d1)/7) * d2) (((3*d2)/7) * d1)))
        |> (function Some x -> (3*x)/7 | None -> 0))
