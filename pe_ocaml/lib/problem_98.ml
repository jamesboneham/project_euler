open Core

let p98 () =
  let words = Stdio.In_channel.read_all "resources/p098_words.txt"
              |> String.substr_replace_all ~pattern:"\"" ~with_:""
              |> String.split_on_chars ~on:[',']
  and find_anagrams wlist =
    List.(map wlist ~f:(fun word -> (word, String.to_list word
                                           |> sort ~compare:(Char.compare)
                                           |> String.of_char_list))
          |> sort ~compare:(fun (_,ltrs1) (_,ltrs2) -> String.compare ltrs1 ltrs2)
          |> group ~break:(fun (_,ltrs1) (_,ltrs2) -> String.(ltrs1 <> ltrs2))
          |> filter ~f:(fun l -> (length l) > 1)
          >>| (fun l -> map l ~f:(fun (word, _) -> word)))
  in let anagrams = find_anagrams words
                    |> List.(sort ~compare:(fun x y ->
                        Int.compare (hd_exn y |> String.length)
                          (hd_exn x |> String.length)))
                    |> List.(group ~break:(fun x y ->
                        (String.length (hd_exn x)) <> (String.length (hd_exn y))))
  in let squares = List.(Int.pow 10 (anagrams |> hd_exn |> hd_exn |> hd_exn |> String.length)
                         |> float |> sqrt |> int_of_float |> range 0
                         |> map ~f:(fun x -> x*x |> Int.to_string) |> rev
                         |> group ~break:(fun x y -> (String.length x) <> (String.length y)))
  in let get_perms ans =
       let perm = fun word1 word2 ->
         word2 |> String.to_list |> List.map ~f:(fun c -> String.index_exn word1 c)
       in match ans with
       | car::cdr -> List.map cdr ~f:(fun x -> perm car x)
       | _ -> []
  and get_possiblesqrs = fun ans ->
    let an0 = List.hd_exn ans
    in let sqrs = List.find squares ~f:(fun x -> (String.length (List.hd_exn x)) = (String.length an0))
    in let nunique = fun s -> List.(s |> String.to_list |> stable_dedup |> length)
    in match sqrs with
    | Some x -> List.filter x ~f:(fun sqr -> (nunique sqr) = (nunique an0))
    | None -> []
  in let check_ans = fun ans ->
      let allwdsqrs = get_possiblesqrs ans
      and perms = get_perms ans
      in let check_square sqr =
           List.filter_map perms
             ~f:(fun inds -> List.(map inds ~f:(fun i -> String.nget sqr i)
                                   |> String.of_char_list
                                   |> (fun numstr -> find allwdsqrs ~f:(fun x -> String.(=) x numstr))))
      in List.concat_map allwdsqrs ~f:check_square
  in List.fold_until anagrams ~init:[] ~finish:(fun _ -> 0)
    ~f:(fun _ ans -> match (List.concat_map ans ~f:check_ans) with
        | [] -> Continue []
        | x -> Stop List.(x >>| Int.of_string
                          |> max_elt ~compare:Int.compare
                          |> function Some x -> x | None -> 0))
