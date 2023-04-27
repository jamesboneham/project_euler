let p7 = fun m ->
  let nmax = (m |> float |> (fun x -> x*.((log x) +. (log (log x)))) |> int_of_float) + 5
  in let prime_arr = Array.make (nmax+1) true
  and sqrtn = 1 + (nmax |> float |> sqrt |> int_of_float)
  in let store_arr = (prime_arr.(0) <- false; prime_arr.(1) <- false;[|(0, ([]:int list))|])
  and update = fun n (cntr, plist) -> (cntr + 1, n::plist)
  in let rec sieve_fun = fun j i ->
      match j with
      | x when x > nmax -> ()
      | _ -> sieve_fun (prime_arr.(j) <- false; (j+i)) i
  in let iter_fun = fun n tf ->
      match tf with
      | false -> ()
      | true when n<sqrtn -> sieve_fun (store_arr.(0)<-(update n store_arr.(0)); (n*n)) n
      | true -> store_arr.(0)<-(update n store_arr.(0))
  in Array.iteri iter_fun prime_arr;
  (fun (n, x) -> List.nth x (n-m)) store_arr.(0)

let%test "basic test" = (p7 6) = 13
let%test "gives correct answer" = (p7 10001) = 104743
