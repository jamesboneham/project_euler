open Core

let p91 nmax =
  let pred x0 y0 x =
    if x = x0 then false else
      let x2xx = x0*x0 - x0*x in
      (if x2xx mod y0 <> 0 then false else
         ((y0 + x2xx/y0) |> (fun g -> g <= nmax && g >= 0)))
  and ints = List.range 0 (nmax+1)
  in List.((concat (init nmax ~f:(fun i -> init nmax ~f:(fun j -> (i+1, j+1)))))
           |> (fold ~init:(3*nmax*nmax)
                 ~f:(fun ntri (x0, y0) ->
                     ntri + (length (filter ints ~f:(fun x -> pred x0 y0 x))))))
