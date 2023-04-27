open Core
let p22 = fun () ->
  let score_map = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  |> String.to_list
                  |> List.foldi ~init:(Map.empty (module Char))
                    ~f:(fun i map letter -> Map.set map ~key:letter ~data:(i+1))
  in let wordscore w = List.(String.(to_list w) >>| (fun c -> Map.find_exn score_map c)
                             |> reduce_balanced_exn ~f:(+))
  in let listscore wl = List.foldi wl ~init:0 ~f:(fun i score w ->
      score + ((wordscore w) * (i+1)))
  in Stdio.In_channel.read_all "resources/p022_names.txt"
     |> String.substr_replace_all ~pattern:"\"" ~with_:""
     |> String.split_on_chars ~on:[',']
     |> List.sort ~compare:String.compare
     |> listscore
