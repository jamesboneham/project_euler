open Core

(* Down to under a second now---I guess that's ok... *)

let p74 () =
  let nmax = 1000000
  and clen = 60
  and facmap =
    let dfactalist =
      let fact =
        function 0 -> 1
               | n -> List.(init n ~f:(fun x -> x+1) |> reduce_exn ~f:( * ))
      and tochar =
        fun n -> n |> Int.to_string |> Char.of_string
      in List.(init 10 ~f:(fun x -> x) >>| (fun n -> (tochar n, fact n)))
    in  match Char.Map.of_alist dfactalist with
    | `Ok x -> x
    | `Duplicate_key y -> raise (Invalid_argument (sprintf "Error: key '%s' is a duplicate" (String.of_char y)))
  in let getfac = fun c ->
      match Char.Map.find facmap c with
      | Some x -> x
      | None -> raise (Invalid_argument (sprintf "Error: key '%s' is not a digit" (String.of_char c)))
  in let sumfacs n =
       String.fold (Int.to_string n) ~init:0 ~f:(fun n c -> (+) n @@ getfac c)
  and lenhash = Hashtbl.create (module Int)
  in let rec buildchain n chain len =
       sumfacs n
       |> fun x ->  Hashtbl.find_and_call lenhash x
         ~if_found:(fun y ->
             List.fold ~init:(y+1, false) ~f:(fun (acc,brk) z -> Hashtbl.set lenhash ~key:z ~data:acc; (acc+1,brk)) chain)
         ~if_not_found:(fun y ->
             match y with
             | z when not @@ List.mem chain z ~equal:(=) -> buildchain z (z :: chain) (len+1)
             | _ -> List.rev chain
                    |> List.fold ~init:(len, false)
                      ~f:(fun (len,brk) z -> (Hashtbl.set lenhash ~key:z ~data:len;
                                              (if brk || z=y then len else len - 1), brk || z=y)))
  in let rec getchains n nchains =
       if n >= nmax
       then nchains
       else match Hashtbl.find lenhash n with
         | Some v -> getchains (n+1) (if v >= clen then nchains+1 else nchains)
         | None -> (ignore ((buildchain n [n] 1) : int * bool);
                    getchains n nchains)
  in getchains 1 0
