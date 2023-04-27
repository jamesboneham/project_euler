open Core
let p59 () =
  let code = In_channel.read_all "resources/p059_cipher.txt"
             |> String.split ~on:','
             |> List.map ~f:Int.of_string
  and ascii =
    String.to_list (" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    ^"[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")
    |> List.mapi ~f:(fun i c -> (i+32, c))
    |> List.filter ~f:(fun (_,x) -> not String.(contains "#$%@\\^_`{|}~" x))
    |> Map.of_alist_exn (module Int)
  in
  let check_val chars value =
    let xor = (lxor) value in
    List.fold_until chars ~init:() ~finish:(fun () -> Some value)
      ~f:(fun () c -> match (Map.mem ascii @@ xor c) with
          | false -> Stop None
          | true -> Continue ())
  and poss_vals = List.range 97 123
  in
  let allowed_vals chars =
    List.filter_map ~f:(check_val chars) poss_vals
  and chars1,chars2,chars3 =
    List.foldi code ~init:([],[],[]) ~f:(fun i (c1,c2,c3) n ->
        match i%3 with
        | 0 -> (n::c1,c2,c3)
        | 1 -> (c1,n::c2,c3)
        | _ -> (c1,c2,n::c3))
  in
  let allowed_keys =
    let allwd_1 = allowed_vals chars1
    and allwd_2 = allowed_vals chars2
    and allwd_3 = allowed_vals chars3
    in
    List.(concat_map allwd_1
            ~f:(fun c1 -> concat_map allwd_2
                   ~f:(fun c2 -> map allwd_3
                          ~f:(fun c3 -> [c1;c2;c3]))))
  in
  let decode key =
    let rec loop msg k cphr = match (k,cphr) with
      | (_, []) -> msg |> List.reduce_exn ~f:(+)
      | ([], _) -> loop msg key cphr
      | (x::cdr1, y::cdr2) -> loop ((x lxor y) :: msg) cdr1 cdr2
    in loop [] key code
  in
  decode List.(hd_exn allowed_keys)
