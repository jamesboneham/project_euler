open Core
let p89 () =
  let rnums = Stdio.In_channel.read_all "resources/p089_roman.txt"
              |> String.substr_replace_all ~pattern:"IV" ~with_:"U"
              |> String.substr_replace_all ~pattern:"IX" ~with_:"W"
              |> String.substr_replace_all ~pattern:"XL" ~with_:"K"
              |> String.substr_replace_all ~pattern:"XC" ~with_:"B"
              |> String.substr_replace_all ~pattern:"CD" ~with_:"E"
              |> String.substr_replace_all ~pattern:"CM" ~with_:"N"
              |> String.split_on_chars ~on:['\n']
  and rom2dec = Char.Map.of_alist
      [('I',(1,1)); ('U',(2,4)); ('V',(1,5)); ('W',(2,9)); ('X',(1,10)); ('K',(2,40)); ('L',(1,50));
       ('B',(2,90)); ('C',(1,100)); ('E',(2,400)); ('D',(1,500)); ('N',(2,900)); ('M',(1,1000))]
                |> function `Ok x -> x
                          | `Duplicate_key (_ : char) -> Char.Map.empty
  and dec2romlst = [(1000,"M"); (900,"CM"); (500,"D"); (400,"CD"); (100,"C"); (90,"XC");
                    (50,"L"); (40,"XL"); (10,"X"); (9,"IX"); (5,"V"); (4,"IV"); (1,"I")]
  in let parse rnum =
       String.fold rnum ~init:(0,0)
         ~f:(fun (cumlen, cumn) c ->
             Char.Map.find rom2dec c
             |> function Some (len, n) -> (cumlen+len, cumn+n)
                       | None -> (cumlen, cumn))
  in let rec dec2rom nrom nres nlst =
       match (nres, nlst) with
       | (0, _) -> nrom
       | (x, (n, (_ : string)) :: cdr) when n>x -> dec2rom nrom nres cdr
       | (x, (n, s) :: (_ : (int * string) list)) when (n = 500 || n = 400) ->
         dec2rom (nrom^s) (x-n) List.(slice dec2romlst 4 (length dec2romlst))
       | (x, (n, s) :: (_ : (int * string) list)) when (n = 50 || n = 40) ->
         dec2rom (nrom^s) (x-n) List.(slice dec2romlst 6 (length dec2romlst))
       | (x, (n, s) :: (_ : (int * string) list)) when (n = 5 || n = 4) ->
         dec2rom (nrom^s) (x-n) List.(slice dec2romlst 10 (length dec2romlst))
       | (x, (n, s) :: (_ : (int * string) list)) -> dec2rom (nrom^s) (x-n) nlst
       | (_ : int * (int * string) list) -> failwith "Shouldn't get here..."
  in let delta_length rnum =
       let (olen, dnum) = parse rnum
       in let newnum = dec2rom "" dnum dec2romlst
       in let dlen = olen - (String.length newnum)
       in if dlen < 0 then failwith "Original shorter than new 'minimum' number..."
       else dlen
  in List.(fold rnums ~init:0 ~f:(fun acc rnum -> acc + (delta_length rnum)))
