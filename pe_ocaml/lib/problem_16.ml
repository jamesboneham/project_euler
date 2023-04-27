open Core
let p16 = fun npower2 ->
  npower2
  |> Big_int.power_int_positive_int 2
  |> Big_int.string_of_big_int
  |> String.to_list
  |> List.fold_left ~init:0  ~f:(fun y x -> x
                                            |> Char.to_string
                                            |> Int.of_string
                                            |> (+) y)
