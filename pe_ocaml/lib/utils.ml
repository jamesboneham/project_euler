open! Core

let quicktime = fun f x ->
  let t = Time_ns.(now () |> to_int_ns_since_epoch) in
  let res = f x in
  Printf.printf "Execution time: %fs\n"
    (Time_ns.(now () |> to_int_ns_since_epoch) - t
     |> float |> ( *. ) (10. ** -9.));
  res

let timeit = fun func arg ->
  let tns = fun () -> Time_ns.(now () |> to_int_ns_since_epoch)
  in let time1loop = fun () ->
      let t0 = tns ()
      in let (_ : 'a) = func arg
      in tns () - t0
  and timeinit = fun () ->
    let t0 = tns ()
    in let res = func arg
    in let telapsed = tns () - t0
    in match telapsed with
    | x when x < 10_000 -> (100_000, (fun x -> x |> float |> ( *. ) 1.), "ns", res)
    | x when x < 1_000_000 -> (1_000, (fun x -> x |> float |> ( *. ) (10. ** -3.)), "μs", res)
    | x when x < 1_000_000_000 -> (10, (fun x -> x |> float |> ( *. ) (10. ** -6.)), "ms", res)
    | x -> Printf.printf "Execution time: %f s\t(1 loop)\n"
             (x |> float |> ( *. ) (10. ** -9.)); (0, (fun x -> x |> float |> (+.) 1.), "s", res)
  in let (n, fmt, units, res) = timeinit ()
  in let timeloop = fun () ->
      let n_f = float n
      in let rec innerloop = fun (ex, ex2) ni -> match ni with
          | x when (x = n) -> (ex, sqrt((ex2 -. ex**2.)))
          | x -> innerloop (time1loop ()
                            |> fmt
                            |> (fun y -> (ex +. (y /. n_f), ex2 +. (y**2. /. n_f))))
                   (x+1)
      in innerloop (0., 0.) 0
  in let (mu, sigma) = timeloop ()
  in let outputtime = fun () -> match n with
      | 0 -> ()
      | x -> Printf.(printf "Mean execution time (%d loops) of:\n" x;
                     printf "\t%f %s\t(standard deviation of %f %s)\n" mu units sigma units)
  in outputtime (); Printf.printf "Result obtained:\n"; res

let time_repeat nmax f x =
  let rec repeat num = match num with
    | _ when num >= nmax -> f x
    | _ -> ignore ((f x) : 'a); repeat (num + 1)
  in timeit repeat 0

let timeit_graph = fun func arg ->
  let tns = fun () -> Time_ns.(now () |> to_int_ns_since_epoch)
  in let time1loop = fun () ->
      let t0 = tns ()
      in let (_ : 'a) = func arg
      in let t1 = tns ()
      in t1 - t0
  and timeinit = fun () ->
    let t0 = tns ()
    in let res = func arg
    in let telapsed = tns () - t0
    in match telapsed with
    | x when x < 1_000 -> (100_000, (fun x -> x |> float |> ( *. ) 1.), "ns", res)
    | x when x < 1_000_000 -> (10_000, (fun x -> x |> float |> ( *. ) (10. ** -3.)), "us", res)
    | x when x < 100_000_000 -> (100, (fun x -> x |> float |> ( *. ) (10. ** -6.)), "ms", res)
    | _ -> (0, (fun x -> x |> float), "\n\n***YOU SHOULD NOT SEE THIS MESSAGE***\n\n", res)
  in let (n, fmt, units, res) = timeinit ()
  in if n = 0 then (Printf.printf "\nSending to 'timeit' instead...\n\n"; timeit func arg) else
    let timeloop = fun () ->
      let n_f = float n
      in let times = List.(init n ~f:(fun _ -> fmt @@ time1loop ())
                           |> sort ~compare:Float.compare
                           |> sub ~pos:0 ~len:((99*n)/100))
      in let (mu, sigma) = List.(fold ~init:(0., 0.) times
                                   ~f:(fun (x0,y0) x ->
                                       (x0 +. (x /. n_f), y0 +. ((x**2.) /. n_f))))
                           |> (fun (x,y) -> (x, sqrt(y -. x**2.)))
      in (mu, sigma, times)
    in let mu,_,tlist = timeloop ()
    and nbins = 50
    in let bins =
         List.(let (x0, x1) = tlist |> fun x -> (hd_exn x, last_exn x)
               in let delta = (x1 -. x0) /. (float @@ nbins - 3)
               in let xmin = x0 -. (2. *. delta)
               in init (nbins + 1) ~f:(fun i -> xmin +. (( *. ) delta @@ float i)))
    in let data =
         List.(tlist >>|
               (fun n -> filter bins ~f:(fun x -> Float.(<) x n) |> length |> (+) (-1))
               |> group ~break:(<>) >>| (fun x -> (hd_exn x, length x))
               |> (fun x ->
                   x >>| (fun (y,z) -> Int.(
                       y, of_float (119. *. (to_float z)
                                    /. (max_elt ~compare:(fun (_,y1) (_,y2) -> compare y1 y2) x
                                        |> fun x -> match x with
                                        | Some y -> to_float @@ snd y
                                        | _ -> 0.)) + 1)))
               |> (fun lst ->
                   init 50 ~f:(fun i ->
                       match find lst ~f:(fun (x,_) -> x = i) with
                       | Some x -> x | None -> (i, 0))))
    in let basestr = "|"
                     ^ (String.init nbins ~f:(fun _ -> ' '))
                     ^ "\n"
    and blocks = [" ";"▁";"▂";"▃";"▄";"▅";"▆";"▇";"█"]
    in let plotstr bindata = List.(
        fold bindata ~init:"|" ~f:(fun s (_,x) -> match x with
            | i when i >= 8 -> s ^ nth_exn blocks 8
            | i when i >= 0 -> s ^ nth_exn blocks i
            | _ -> "?") ^ "\n"
      )
    and decrcntr bindata =
      List.(bindata >>| (function (x,y) when (y >= 8) -> (x,y-8) | (x,_) -> (x,0)))
    and get_ticks  = fun () ->
      String.concat
        (List.init 5
           ~f:(fun i -> List.((nth_exn bins (i*10 + 4)) +. nth_exn bins (i*10 + 5)) /. 2.
                        |> Printf.sprintf "%6.2f"
                        |> String.to_list
                        |> (fun l -> List.take l 6)
                        |> String.of_char_list
                        |> (fun s -> Printf.sprintf " %6s %-2s" s units)))
      |> Printf.sprintf "%*s" (String.length basestr)
    in let getplot () =
         let plotbase = [String.concat
                           (List.init 51 ~f:(function 0 -> "└"
                                                    | i when (i mod 10 = 5) -> "┬"
                                                    | _ -> "─")) ^ "\n";
                         Printf.sprintf "%*s" (String.length basestr) "\n";
                         get_ticks ()]
         in let rec buildplot bindata plotlst =
              match List.fold bindata ~init:false ~f:(fun non0 (_,v) -> non0 || (v > 0)) with
              | false -> plotlst
              | true -> buildplot (decrcntr bindata) ((plotstr bindata) :: plotlst)
         in buildplot data plotbase
    and getres () =
      let mustr = Printf.sprintf "%.2f %-2s" mu units
      and loopstr = Printf.sprintf "(%d loops)" n
      in List.(init 18 ~f:(fun i -> match i with
          | 8 -> "  ╔═══════════════════════╗  "
          | 9 -> "  ║ Mean execution time:  ║  "
          | 10 -> Printf.sprintf "  ║%21s  ║  " mustr
          | 11 -> Printf.sprintf "  ║%21s  ║  " loopstr
          | 12 -> "  ╚═══════════════════════╝  "
          | 15 -> "  ╔══════════════╗           "
          | 16 -> "  ║ Result:      ║           "
          | 17 -> "  ╚══════════════╝           "
          | _ -> "                             "))
    in let getoutput () =
         let pstr = getplot ()
         and resstr = getres ()
         in if List.((length pstr) <> (length resstr))
         then List.(fold pstr ~init:"" ~f:(^) |> Printf.printf "\n\n%s\n\n";
                    fold data ~init:"" ~f:(fun s (x,y) -> (^) s @@ Printf.sprintf "(%d,%d) " x y)
                    |> Printf.printf "\n\n%s\n\n";)
         else let outstr = List.map2_exn resstr pstr ~f:(fun s1 s2 -> s1^s2)
                           |> List.reduce_exn ~f:(^)
           in Printf.printf "%s\n  " outstr
    in getoutput (); res

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

let rec gcd a b = match b with | 0 -> a | _ -> gcd b (a mod b)

let combinations pool r =
  let n = List.length pool
  in
  let inds = Array.of_list List.(range 0 r)
  and empty = Int.Set.empty
  in
  let get_comb () = Array.fold inds ~init:empty ~f:(fun comb i ->
      Int.Set.add comb (List.nth_exn pool i))
  in
  let rec iloop combs =
    match List.(find (range ~stride:~-1 (r-1) ~-1) ~f:(fun x -> inds.(x) <> x + n - r)) with
    | None -> combs
    | Some i -> (inds.(i) <- inds.(i) + 1; jloop combs (i+1))
  and jloop combs j = match (j >= r) with
    | true -> iloop ((get_comb ()) :: combs)
    | _ -> (inds.(j) <- inds.(j-1) + 1; jloop combs (j+1))
  in iloop [get_comb ()]

let permutations n lst =
  let perms = ref []
  and arr = Array.of_list lst
  in
  let rec generate k =
    let rec loop i = match i>=k-1 with
      | true -> ()
      | false -> let i0 = (if k mod 2 = 0 then i else 0) in
        let a0 = arr.(i0) and ak = arr.(k-1) in
        (arr.(i0) <- ak; arr.(k-1) <- a0; generate (k-1); loop (i+1))
    in
    if k=1 then perms := Array.(to_list arr) :: !perms
    else (generate (k-1); loop 0)
  in (generate n; !perms)

let to_digs n =
  let rec loop m digs = match (m mod 10, m/10) with
    | (0,0) -> digs
    | (x,y) -> loop y (x::digs)
  in loop n []

let to_num digs =
  let rec loop d n = match d with
    | [] -> n
    | car::cdr -> loop cdr (10*n + car)
  in loop digs 0

let permutations_stream lst =
  let a = ref @@ Array.of_list lst in
  let kmax = Array.(length !a) - 1 in
  let rec findk k1 ak1 = match k1-1 with
    | -1 -> None
    | k -> let ak = !a.(k) in
      (if ak < ak1 then findl k ak kmax else findk k ak)
  and findl k ak l = match !a.(l) with
    | al when al > ak ->
      Some (swap_inds (k + (kmax-k+1)/2) (k+1) kmax;
            !a.(kmax - l + k + 1) <- ak;
            !a.(k) <- al;
            !a)
    | _ -> findl k ak (l-1)
  and swap_inds ilim i j = match i > ilim with
    | true -> ()
    | _ -> let (ai, aj) = (!a.(i), !a.(j)) in
      (!a.(i) <- aj; !a.(j) <- ai; swap_inds ilim (i+1) (j-1))
  in
  let permute (_ : int) =
    match findk kmax !a.(kmax) with
    | Some x -> Some (Array.to_list x)
    | None -> None
  in Stream.(icons lst @@ from permute)

let combinations_stream lst r =
  let n = List.length lst
  and a = ref @@ Array.of_list lst
  and inds = ref @@ Array.of_list List.(range 0 r)
  and r1 = r-1
  in
  let rec iloop i = match !inds.(i) with
    | ii when ii <> (i+n-r) -> (!inds.(i) <- ii+1; jloop (i+1))
    | _ when i = 0 -> None
    | _ -> iloop (i-1)
  and jloop j = match j with
    | k when k >= r -> Some Array.(map !inds ~f:(fun x -> !a.(x)) |> to_list)
    | k -> (!inds.(k) <- !inds.(k-1) + 1; jloop (k+1))
  in
  let combine (_:int) = iloop r1
  in Stream.(icons Array.(map !inds ~f:(fun x -> !a.(x)) |> to_list) @@ from combine)

let get_sigma_finder ulim =
  (* Returns function to find sum of divisors (including n) for numbers less than ulim *)
  let primes = eratosthenes ulim
  in let rec facsum = fun (n,sum) fac pow ->
      match n mod fac = 0 with
      | true -> facsum ((n/fac),(sum + pow)) fac (pow*fac)
      | false -> (n,sum)
  in let sigma n =
       List.fold_until primes ~init:(n,1)
         ~finish:(fun (_,y)-> Printf.printf "%d: Not enough primes, answer likely inaccurate\n" n;
                   y)
         ~f:(fun acctup pfac ->
             match acctup with
             | (1,prod) -> Stop prod
             | (x,prod) when x mod pfac = 0 ->
               Continue ((fun (z,y) -> (z,prod*y)) @@ facsum (x,1) pfac pfac)
             | _ -> Continue acctup)
  in sigma

let get_mr_test_int ?nmax () =
  let two = 2
  and divmod n d = let q = n/d in (q, n - d*q)
  in
  let modpow b0 e0 m =
    let rec loop b r e = Int.(match rem e two with
        | _ when e <= zero -> r
        | x when x=one -> loop (rem (b*b) m) (rem (r*b) m) (e / two)
        | _ -> loop (rem (b*b) m) r (e / two))
    in Int.(loop (rem b0 m) one e0)
  and find_sd n =
    let rec loop d s = match divmod d 2 with
      | (d1, 0) -> loop d1 (s+1)
      | _ -> (s,d)
    in loop (n-1) 0
  and a_bounds = [2_047; 1_373_653; 9_080_191; 25_326_001; 3_215_031_751;
                  4_759_123_141; 1_122_004_669_633; 2_152_302_898_747;
                  3_474_749_660_383; 341_550_071_728_321; 3_825_123_056_546_413_051]
  and a_vals = [[2]; [2;3]; [31;73]; [2;3;5]; [2;3;5;7]; [2;7;61]; [2;13;23;1662803];
                [2;3;5;7;11]; [2;3;5;7;11;13]; [2;3;5;7;11;13;17]; [2;3;5;7;11;13;17;19;23];
                [2;3;5;7;11;13;17;19;23;29;31;37;41]]
  and mr_test avals witfun = List.fold_until avals ~init:() ~finish:(fun _ -> true)
      ~f:(fun () a -> match witfun a with | true -> Continue () | _ -> Stop false)
  in
  let get_avals n = List.(match findi a_bounds ~f:(fun _ lim -> lim > n) with
      | Some (i,_) -> nth_exn a_vals i
      | _ -> last_exn a_vals)
  and get_witfun n s d =
    let get_x0 a = modpow a d n
    in
    let witness a =
      let rec witness_loop x i = match x with
        | _ when i >= s -> false
        | x1 when x1 = n-1 -> true
        | _ -> witness_loop (modpow x 2 n) (i+1)
      in
      let x0 = get_x0 a in
      (x0 = 1 || witness_loop x0 0)
    in witness
  in match nmax with
  | None ->
    let mr n =
      if (n <= 3 || n%2 = 0)
      then
        (if (n=2 || n=3) then true else false)
      else
        (let avals = get_avals n
         and s,d = find_sd n in
         let witfun = get_witfun n s d in
         mr_test avals witfun)
    in mr
  | Some lim ->
    let avals = get_avals lim in
    let mr n =
      if (n <= 3 || n%2 = 0)
      then
        (if (n=2 || n=3) then true else false)
      else
        (let s,d = find_sd n in
         let witfun = get_witfun n s d in
         mr_test avals witfun)
    in mr
