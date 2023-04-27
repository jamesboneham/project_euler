open Core
let p44 () =
  let pents = List.init 2000 ~f:(fun n -> ((n+1)*(3*n+2))/2)
  in
  let pset = Int.Set.of_list pents
  in
  let ispent n =
    (Int.Set.mem pset n) ||
    Float.(of_int Int.(n*24 + 1) |> sqrt |> iround_nearest_exn
           |> Int.(fun x -> (x*x = (24*n+1)) && x mod 6 = 5))
  in
  List.(find_exn pents ~f:(fun pm ->
      match find pents ~f:(fun pk -> ispent (pk+pm) && ispent (2*pk+pm)) with
      | None -> false
      | Some _ -> true))
