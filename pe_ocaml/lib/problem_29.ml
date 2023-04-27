open Core

let p29 = fun () ->
  let nmax = 100
  in let ulim = Float.(nmax |> of_int |> sqrt |> to_int)
  in let rec nrepeats nreps n pwr =
       match (n > ulim) with
       | true -> nreps
       | false when Int.(n ** pwr > nmax) -> nrepeats nreps (n + 1) 2
       | false -> nrepeats (nreps + nmax/pwr - 1) n (pwr + 1)
  in (nmax-1)*(nmax-1) - (nrepeats 0 2 2) + 1
