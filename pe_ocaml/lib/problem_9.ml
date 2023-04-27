let p9 = fun () ->
  let rec finda = fun b ->
    let anum = (500_000 - 1_000*b) and aden = (1_000 - b)
    in match (anum mod aden) with
    | 0 when (anum > 0) -> (fun a -> a*b*(1000-a-b)) (anum/aden)
    | _ -> finda (b+5)
  in finda 5
