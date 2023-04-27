open Core
let p20 = fun nfact ->
  let rec bigfact = fun n fact ->
    Bigint.(match (n, of_int 1) with
        | (x,y) when x=y -> fact |> to_string
        | (x,y) -> bigfact (x-y) (fact*x))
  in Bigint.(bigfact (of_int nfact) (of_int 1))
     |> String.to_list
     |> List.map ~f:(fun c -> c |> String.of_char |> Int.of_string)
     |> List.fold_left ~init:0 ~f:(+)
