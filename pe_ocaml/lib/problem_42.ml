open Core
let p42 () =
  let score_map = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  |> String.to_list
                  |> List.foldi ~init:Char.Map.empty
                    ~f:(fun i map letter -> Map.set map ~key:letter ~data:(i+1))
  and trinums = List.(range 1 20 >>| (fun n -> n*(n+1)/2) |> Hash_set.of_list (module Int))
  in
  let is_triword word =
    let score c = match Char.Map.find score_map c with
      | Some x -> x
      | None -> 0
    in String.fold word ~init:0 ~f:(fun acc c -> acc + (score c))
       |> Hash_set.mem trinums
  in
  Stdio.In_channel.read_all "resources/p042_words.txt"
  |> String.substr_replace_all ~pattern:"\"" ~with_:""
  |> String.split_on_chars ~on:[',']
  |> List.filter ~f:is_triword
  |> List.length
