open Core
module Iset = Set.Make(Int)
let p32 = fun () ->
  let char2int = Char.Map.of_alist_exn
      [('1',1);('2',2);('3',3);('4',4);('5',5); ('6',6);('7',7);('8',8);('9',9)]
  and digits = Iset.of_list(List.range 1 10)
  and ispandig = fun perm ~n -> match perm with
    | 9::_ | 8::_ -> false
    | [a;b;c;d;e] when ((1000*a*c + 100*(b*c + a*d) + 10*(a*e+b*d) + b*e = n)
                        || (1000*a*b + 100*a*c + 10*a*d + a*e = n)) -> true
    | _ -> false
  and ( ^^ ) e ll = List.map ~f:(fun x -> e::x) ll
  in let rec permut l r =
       match r with
       | [] -> [[]]
       | [x] -> x ^^ (permut [] l)
       | x::t -> let s = permut (x::l) t in
         (x ^^ (permut [] (l@t))) @ s
  in let getdigs = fun n ->
      n |> Int.to_string |> String.to_list
      |> (fun x -> if List.mem x '0' ~equal:Char.(=)
                   || List.contains_dup ~compare:Char.compare x then []
           else x |> List.map ~f:(fun c -> Map.find_exn char2int c)
                |> Iset.of_list |> Iset.diff digits |> Iset.to_list)
  in let testnum = fun n ->
      let digs = getdigs n
      in match digs with
      | [] -> false
      | _ -> List.fold ~init:false ~f:(fun x y -> x || y)
               List.(permut [] digs >>| ispandig ~n:n)
  in List.(range 1234 9876 |> filter ~f:testnum |> reduce_exn ~f:(+))
