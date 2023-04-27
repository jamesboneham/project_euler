open Core
let p54 () =
  let listcomp lst1 lst2 =
    let rec loop l1 l2 = match (l1,l2) with
      | (car1::cdr1, car2::cdr2) -> (match Int.compare car1 car2 with
          | 0 -> loop cdr1 cdr2
          | x -> x)
      | _ -> 0
    in loop lst1 lst2
  and hands = In_channel.read_lines "resources/p054_poker.txt"
  and card_vals = Map.of_alist_exn (module String)
      List.(init 13 ~f:(fun i -> match i+2 with
          | 10 -> ("T", 10)
          | 11 -> ("J", 11)
          | 12 -> ("Q", 12)
          | 13 -> ("K", 13)
          | 14 -> ("A", 14)
          | x -> (Int.to_string x, x)))
  in
  let handslist =
    List.(hands >>| (fun s ->
        String.(split s ~on:' ')
        |> groupi ~break:(fun i _ _ -> i=5)))
  and process_hand hand =
    let vals = List.(hand >>| (fun s -> String.prefix s 1
                                        |> Map.find_exn card_vals)
                     |> sort ~compare:(fun x y -> Int.compare y x))
    in
    let is_run =
      List.fold_until vals ~init:0 ~finish:(fun x -> (x+4))
        ~f:(fun acc v -> match acc with
            | x when (x = 0 || x = v+1) -> Continue v
            | _ -> Stop 0)
    and is_flush =
      List.(if hand >>| (fun s -> String.slice s 1 2)
               |> dedup_and_sort ~compare:String.compare
               |> length |> (=) 1
            then (hd_exn vals)
            else 0)
    and dupes = List.(find_all_dups vals ~compare:Int.compare
                      >>| (fun v -> (count vals ~f:((=) v), v)))
    in
    let is_four = match dupes with
      | (4, x) :: [] -> x
      | _ -> 0
    and is_fullhouse = match dupes with
      | ((3,x) :: (2,y) :: []) | ((2,y) :: (3,x) :: []) -> (100*x + y)
      | _ -> 0
    and is_three = match dupes with
      | (3, x) :: [] -> x
      | _ -> 0
    and is_twopair = match dupes with
      | (2,x) :: (2,y) :: [] -> (100*x + y)
      | _ -> 0
    and is_pair = match dupes with
      | (2, x) :: [] -> x
      | _ -> 0
    in ([(if is_run > 0 then is_flush else 0); is_four; is_fullhouse; is_flush;
         is_run; is_three; is_twopair; is_pair], vals)
  and compare_hands (h1,v1) (h2,v2) = match listcomp h1 h2 with
    | 1 -> 1
    | 0 -> if (listcomp v1 v2) = 1 then 1 else 0
    | _ -> 0
  in
  List.fold handslist ~init:0 ~f:(fun acc round -> match round with
      | h1::h2::[] -> acc + (compare_hands (process_hand h1) (process_hand h2))
      | _ -> acc)
