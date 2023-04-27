open Core
let p35 () =
  let nmax = 1000000 in
  let pset = Int.Set.of_list (Utils.eratosthenes nmax)
  in
  let get_digs n =
    let rec loop digs m = match (m mod 10, m/10) with
      | (0, 0) -> digs
      | (x, y) -> loop (x::digs) y
    in loop [] n
  in
  let forbidden = Int.Set.of_list [0;2;4;5;6;8]
  in
  let cache = Hash_set.create (module Int)
  in
  let check_prime p =
    let digs = get_digs p
    in
    if Option.is_some List.(find digs ~f:Int.Set.(mem forbidden)) then false
    else
      let nrot = List.length digs in
      let pwr10 = Int.(pow 10 (nrot - 1)) in
      let rec loop prm n =
        if n >= nrot then (Hash_set.add cache p; true) else
          match (prm mod 10)*pwr10 + prm/10 with
          | p1 when Int.Set.mem pset p1 -> loop p1 (n+1)
          | _ -> false
      in
      loop p 1
  in
  Int.Set.fold pset ~init:2 (* to account for 2 and 5 which are both forbidden *)
    ~f:(fun acc p -> if (Hash_set.mem cache p || check_prime p)
         then acc + 1 else acc)
