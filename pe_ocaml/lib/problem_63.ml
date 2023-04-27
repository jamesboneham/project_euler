open Core
let p63 () =
  let np n = Float.((log 10.)/((log 10.) - (log @@ of_int n)) |> iround_down_exn) in
  List.(range 1 10 |> fold ~init:0 ~f:(fun x n -> x + (np n)))
