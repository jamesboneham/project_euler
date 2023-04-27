open Core

let p25 = fun ndigs ->
  let fib0 = Array.create ~len:ndigs 0
  and fib1 = Array.create ~len:ndigs 0
  and arradd = fun arr1 arr2 ->
    Array.fold2_exn arr1 arr2 ~init:(0,0)
      ~f:(fun (i,rem) x y -> let sum = rem + x + y in arr2.(i) <- sum mod 10; (i+1,sum/10))
    |> (fun (x : int * int) -> ignore x)
  in let rec fibnext (i,f_i,f_prev) =
       match f_i.(ndigs-1) with
       | 0 -> arradd f_i f_prev; fibnext (i+1, f_prev, f_i)
       | _ -> i
  in fib1.(0) <- 1;
  fibnext (1,fib1,fib0)
