open Core

let p79 = fun () ->
  let guesses = Stdio.In_channel.read_lines "resources/p079_keylog.txt"
  and cdigs = List.init ~f:(fun x -> x |> string_of_int |> Char.of_string) 10
  and onlyfirst n glist =
    List.fold_until glist ~finish:(fun x->x) ~init:false
      ~f:(fun first numstr -> match String.suffix numstr 2 with
          | x when String.mem x n -> Stop false
          | _ when first -> Continue true
          | _ -> Continue Char.(String.get numstr 0 = n))
  and cyclenums nfirst glist =
    List.map glist ~f:(fun numstr -> match String.get numstr 0 with
        | x when Char.(x = nfirst) -> String.(suffix numstr 2 ^ "~")
        | _ -> numstr)
  in let rec getfirst (passcode, glist) =
       match
         List.(fold_until cdigs ~init:("",[]) ~finish:(fun _ -> (passcode, glist))
                 ~f:(fun _ c ->
                     if onlyfirst c glist
                     then Stop (passcode ^ String.of_char c, cyclenums c glist)
                     else Continue ("",[])))
       with
       | (x,_) when String.(x = passcode) -> passcode
       | y -> getfirst y
  in getfirst ("", guesses)
