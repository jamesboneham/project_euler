open! Core
let p14 = fun nmax ->
  let cands = Array.(create ~len:nmax true)
  in let rec collatz = fun num nterms ->
      if num < nmax then cands.(num) <- false else ();
      match num with
      | 1 -> nterms
      | x when (x mod 2 = 0) -> collatz (num/2) (nterms+1)
      | _ -> collatz (3*num + 1) (nterms+1)
  and get_max = fun num (maxlen,maxnum) ->
      match num with
      | 1 -> (maxlen,maxnum)
      | x when (x>=nmax || cands.(x)) ->
        let collen = collatz num 1
        in get_max (num-1) (if maxlen > collen then (maxlen,maxnum) else (collen,num))
      | _ -> get_max (num-1) (maxlen,maxnum)
  in get_max nmax (0,0) |> snd
