open Core
let p15 = fun nmax ->
  let ijmax = nmax + 1
  and ulim = (nmax + 1)*(nmax + 1) - 1
  in let rec buildlist = fun n pathlist ->
    match (n/ijmax,n mod ijmax,pathlist) with
      | (_,_,car::cdr) when n = ulim -> car + List.(last_exn cdr)
      | (0,_,_) -> buildlist (n+1) (1::pathlist)
      | (_,0,_) -> buildlist (n+1) (1::List.(drop_last_exn pathlist))
      | (_,_,car::cdr) ->
        buildlist (n+1) List.((car + (last_exn cdr))::(drop_last_exn pathlist))
      | _ -> 0
    in buildlist 0 []
