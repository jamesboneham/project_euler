open Core

let p30 = fun () ->
  let scores = Map.of_alist_exn (module Char)
      (List.init 10 ~f:(fun x -> Int.((Char.of_string (to_string x),x**5))))
  in let is5powsum = fun n ->
      n |> Int.to_string
      |> String.to_list
      |> List.fold ~init:0
        ~f:(fun x y -> ((Map.find_exn scores y) + x))
      |> (=) n
  in let rec get5powsum = fun sum n -> match n > 354295 with
      | true -> sum
      | false -> get5powsum (if is5powsum n then sum+n else sum) (n+1)
  in get5powsum 0 1000
