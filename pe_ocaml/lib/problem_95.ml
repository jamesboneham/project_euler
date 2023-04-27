open Core

let p95 nmax =
  let eratosthenes = fun ?(rev = false) nmax ->
    (* Sieve of Eratosthenes implementation *)
    let ulim = nmax |> float |> sqrt |> int_of_float
    and i_nil = fun _ -> ()
    in let mask = Array.create ~len:nmax true
    in let rec sieve = fun primes i ->
        if i >= nmax then (if rev then primes else List.rev primes) else
          match mask.(i) with
          | true when i > ulim -> sieve (i::primes) (i+1)
          | true -> (inner_sieve i i*i |> i_nil;
                    sieve (i::primes) (i+1))
          | false -> sieve primes (i+1)
    and inner_sieve = fun j0 j ->
      match j >= nmax with
      | true -> 0
      | false -> (mask.(j) <- false; inner_sieve j0 (j0+j))
    in sieve [] 2
  in let getmapfun lim =
    let lim2 = lim/2
    in let primes = eratosthenes lim2
    and sntbl = Hashtbl.create (module Int)
    and arr = Array.create 1 ~len:lim
    in arr.(0) <- 0; arr.(1) <- 0;
    let hashtbl2mapfun tbl =
      let mapfun key = match Hashtbl.find tbl key with
        | Some x -> x
        | None -> 0
      in mapfun
    and build_tbl () = Array.iteri arr
        ~f:(fun n sn -> match (n, sn) with
            | (x,y) when ((x >= y && (not (Hashtbl.mem sntbl y)))
                          || (y > lim) || (x = 0)) -> ()
            | (x,y) -> Hashtbl.set sntbl ~key:x ~data:y)
    and getax n x =
      let rec getaxloop a m = match (m mod x) with
        | 0 -> getaxloop (a+1) (m/x)
        | _ -> a
      in getaxloop 0 n
    and advance_s = fun x ax sn n ->
      let x_axp1 = Int.pow x (ax+1) in
      sn + ((x-1)*(x_axp1*sn + n))/(x_axp1 - 1)
    in let primeloop = fun () ->
        let lim = Array.length arr in
        Array.iteri arr
          ~f:(fun i sn ->
              if (i <= 1 || i >= (lim/2)) then ()
              else List.fold_until primes ~init:() ~finish:ignore
                  ~f:(fun () p -> let ip = i*p in
                       if ip >= lim
                       then Stop ()
                       else Continue (match arr.(ip) with
                           | 1 -> (arr.(ip) <- advance_s p (getax i p) sn i)
                           | _ -> ())))
    in primeloop (); build_tbl ();
    hashtbl2mapfun sntbl
  in let mapfun = getmapfun nmax
  in let get_chain num =
       let rec extract_chain l = match l with
         | [] -> []
         | car::cdr -> if car = (List.last_exn cdr) then cdr else extract_chain cdr
       in let rec repeat n chain = match chain with
           | 0::_ -> []
           | x::[] -> repeat 0 ((mapfun x) :: chain)
           | car::cdr -> if (List.mem cdr car ~equal:(=)) then chain
             else repeat (n+1) ((mapfun car) :: chain)
           | _ -> []
       in repeat 0 [num] |> List.rev |> extract_chain
          |> List.(function [] -> (num, 0) | x -> (last_exn x, length x))
  in let rec find_maxchain n nmin chlen = match get_chain n with
      | _ when n >= nmax -> (nmin, chlen)
      | (x, c) when c > chlen -> find_maxchain (n+1) x c
      | _ -> find_maxchain (n+1) nmin chlen
  in find_maxchain 2 0 0 |> fst
