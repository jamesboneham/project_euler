open Core
let p19 = fun () ->
  let rec soln = fun date nSundays ->
    Date.(match year date with
        | x when Int.(x > 2000) -> nSundays
        | _ -> soln (add_months date 1) (if Day_of_week.(=) (day_of_week date) Day_of_week.Sun
                                         then nSundays + 1 else nSundays))
  in soln (Date.of_string "01/01/1901") 0
