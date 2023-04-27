let p10 = fun nmax ->
  let prime_arr = Array.make (nmax+1) true
  and sqrtn = 1+(nmax |> float |> sqrt |> int_of_float)
  in let store_arr = (prime_arr.(0) <- false; prime_arr.(1) <- false; [|0;|])
  in let rec sieve_fun = fun j i ->
      match j with
      | x when x > nmax -> ()
      | _ -> sieve_fun (prime_arr.(j) <- false; (j+i)) i
  in let iter_fun = fun n tf ->
      match tf with
      | false -> ()
      | true when n<sqrtn -> sieve_fun (store_arr.(0)<-(store_arr.(0)+n); (n*n)) n
      | true -> store_arr.(0)<-(store_arr.(0)+n)
  in Array.iteri iter_fun prime_arr;
  store_arr.(0)

let%test "basic test" = (p10 10) = 17
let%test "gives correct answer" = (p10 2_000_000) = 142_913_828_922
