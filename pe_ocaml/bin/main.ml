open Lib
open Core

let _ =
  let rec main () =
    Printf.printf "%s" "Enter problem number: ";
    Out_channel.(flush stdout);
    let in_str = In_channel.(input_line_exn stdin) |> String.strip
    and timeit = Utils.timeit_graph
    (* and quicktime = Utils.quicktime *)
    in let isnum = String.(strip in_str
                           |> strip ~drop:(fun c -> mem "0123456789" c)
                           |> length) = 0
    in let pnum = if isnum then Int.of_string in_str else 0
    in let soln = match (pnum : int) with
        | 0 -> ""
        | 1 -> string_of_int @@ timeit Problem_1.p1 1_000
        | 2 -> string_of_int @@ timeit Problem_2.p2 4_000_000
        | 3 -> string_of_int @@ timeit Problem_3.p3 600_851_475_143
        | 4 -> string_of_int @@ timeit Problem_4.p4 ()
        | 5 -> string_of_int @@ timeit (fun x -> List.reduce_exn ~f:( * ) x) [2;2;2;2;3;3;5;7;11;13;17;19]
        | 6 -> string_of_int @@ timeit Problem_6.p6 100
        | 7 -> string_of_int @@ timeit Problem_7.p7 10_001
        | 8 -> string_of_int @@ timeit Problem_8.p8 ()
        | 9 -> string_of_int @@ timeit Problem_9.p9 ()
        | 10 -> string_of_int @@ timeit Problem_10.p10 2_000_000
        | 11 -> string_of_int @@ timeit Problem_11.p11 ()
        | 12 -> string_of_int @@ timeit Problem_12.p12 500
        | 13 -> string_of_int @@ timeit Problem_13.p13 ()
        | 14 -> string_of_int @@ timeit Problem_14.p14 1_000_000
        | 15 -> string_of_int @@ timeit Problem_15.p15 20
        | 16 -> string_of_int @@ timeit Problem_16.p16 1_000
        | 17 -> string_of_int @@ timeit Problem_17.p17 1_000
        | 18 -> string_of_int @@ timeit Problem_18.p18 ()
        | 19 -> string_of_int @@ timeit Problem_19.p19 ()
        | 20 -> string_of_int @@ timeit Problem_20.p20 100
        | 21 -> string_of_int @@ timeit Problem_21.p21 10_000
        | 22 -> string_of_int @@ timeit Problem_22.p22 ()
        | 23 -> string_of_int @@ timeit Problem_23.p23 28_123
        | 24 -> timeit Problem_24.p24 1_000_000
        | 25 -> string_of_int @@ timeit Problem_25.p25 1_000
        | 26 -> string_of_int @@ timeit Problem_26.p26 1_000
        | 27 -> string_of_int @@ timeit Problem_27.p27 ()
        | 28 -> string_of_int @@ timeit Problem_28.p28 ()
        | 29 -> string_of_int @@ timeit Problem_29.p29 ()
        | 30 -> string_of_int @@ timeit Problem_30.p30 ()
        | 31 -> string_of_int @@ timeit Problem_31.p31 ()
        | 32 -> string_of_int @@ timeit Problem_32.p32 ()
        | 33 -> string_of_int @@ timeit Problem_33.p33 ()
        | 34 -> string_of_int @@ timeit Problem_34.p34 ()
        | 35 -> string_of_int @@ timeit Problem_35.p35 ()
        | 36 -> string_of_int @@ timeit Problem_36.p36 ()
        | 37 -> string_of_int @@ timeit Problem_37.p37 ()
        | 38 -> string_of_int @@ timeit Problem_38.p38 ()
        | 39 -> string_of_int @@ timeit Problem_39.p39 ()
        | 40 -> string_of_int @@ timeit Problem_40.p40 ()
        | 41 -> string_of_int @@ timeit Problem_41.p41 ()
        | 42 -> string_of_int @@ timeit Problem_42.p42 ()
        | 43 -> string_of_int @@ timeit Problem_43.p43 ()
        | 44 -> string_of_int @@ timeit Problem_44.p44 ()
        | 45 -> string_of_int @@ timeit Problem_45.p45 ()
        | 46 -> string_of_int @@ timeit Problem_46.p46 ()
        | 47 -> string_of_int @@ timeit Problem_47.p47 ()
        | 48 -> string_of_int @@ timeit Problem_48.p48 ()
        | 49 -> string_of_int @@ timeit Problem_49.p49 ()
        | 50 -> string_of_int @@ timeit Problem_50.p50 ()
        | 51 -> string_of_int @@ timeit Problem_51.p51 ()
        | 52 -> string_of_int @@ timeit Problem_52.p52 ()
        | 53 -> string_of_int @@ timeit Problem_53.p53 ()
        | 54 -> string_of_int @@ timeit Problem_54.p54 ()
        | 55 -> string_of_int @@ timeit Problem_55.p55 ()
        | 56 -> string_of_int @@ timeit Problem_56.p56 ()
        | 57 -> string_of_int @@ timeit Problem_57.p57 ()
        | 58 -> string_of_int @@ timeit Problem_58.p58 ()
        | 59 -> string_of_int @@ timeit Problem_59.p59 ()
        | 60 -> string_of_int @@ timeit Problem_60.p60 ()
        | 61 -> string_of_int @@ timeit Problem_61.p61 ()
        | 62 -> string_of_int @@ timeit Problem_62.p62 ()
        | 63 -> string_of_int @@ timeit Problem_63.p63 ()
        | 64 -> string_of_int @@ timeit Problem_64.p64 ()
        | 65 -> string_of_int @@ timeit Problem_65.p65 ()
        | 66 -> string_of_int @@ timeit Problem_66.p66 ()
        | 67 -> string_of_int @@ timeit Problem_67.p67 ()
        | 68 -> string_of_int @@ timeit Problem_68.p68 ()
        | 69 -> string_of_int @@ timeit Problem_69.p69 ()
        | 70 -> string_of_int @@ timeit Problem_70.p70 ()
        | 71 -> string_of_int @@ timeit Problem_71.p71 ()
        | 72 -> string_of_int @@ timeit Problem_72.p72 ()
        | 73 -> string_of_int @@ timeit Problem_73.p73 ()
        | 74 -> string_of_int @@ timeit Problem_74.p74 ()
        | 75 -> string_of_int @@ timeit Problem_75.p75 1_500_000
        | 76 -> string_of_int @@ timeit Problem_76.p76 ()
        | 77 -> string_of_int @@ timeit Problem_77.p77 5_000
        | 78 -> string_of_int @@ timeit Problem_78.p78 ()
        | 79 -> timeit Problem_79.p79 ()
        | 80 -> string_of_int @@ timeit Problem_80.p80 ()
        | 86 -> string_of_int @@ timeit Problem_86.p86 1_000_000
        | 87 -> string_of_int @@ timeit Problem_87.p87 50_000_000
        | 88 -> string_of_int @@ timeit Problem_88.p88 12_000
        | 89 -> string_of_int @@ timeit Problem_89.p89 ()
        | 91 -> string_of_int @@ timeit Problem_91.p91 50
        | 92 -> string_of_int @@ timeit Problem_92.p92 10_000_000
        | 93 -> string_of_int @@ timeit Problem_93.p93 ()
        | 94 -> string_of_int @@ timeit Problem_94.p94 1_000_000_000
        | 95 -> string_of_int @@ timeit Problem_95.p95 1_000_000
        | 96 -> string_of_int @@ timeit Problem_96.p96 ()
        | 97 -> string_of_int @@ timeit Problem_97.p97 ()
        | 98 -> string_of_int @@ timeit Problem_98.p98 ()
        | 99 -> string_of_int @@ timeit Problem_99.p99 ()
        | 100 -> string_of_int @@ timeit Problem_100.p100 ()
        | 101 -> Redacted.p101 ()
        | 102 -> Redacted.p102 ()
        | 103 -> Redacted.p103 ()
        | 104 -> Redacted.p104 ()
        | 105 -> Redacted.p105 ()
        | 106 -> Redacted.p106 ()
        | 107 -> Redacted.p107 ()
        | 108 -> Redacted.p108 ()
        | 109 -> Redacted.p109 ()
        | 110 -> Redacted.p110 ()
        | 111 -> Redacted.p111 ()
        | 112 -> Redacted.p112 ()
        | 113 -> Redacted.p113 ()
        | 114 -> Redacted.p114 ()
        | 115 -> Redacted.p115 ()
        | 116 -> Redacted.p116 ()
        | 117 -> Redacted.p117 ()
        | 118 -> Redacted.p118 ()
        | 119 -> Redacted.p119 ()
        | 120 -> Redacted.p120 ()
        | 121 -> Redacted.p121 ()
        | 122 -> Redacted.p122 ()
        | 123 -> Redacted.p123 ()
        | 124 -> Redacted.p124 ()
        | 125 -> Redacted.p125 ()
        | 126 -> Redacted.p126 ()
        | 127 -> Redacted.p127 ()
        | 128 -> Redacted.p128 ()
        | 129 -> Redacted.p129 ()
        | 130 -> Redacted.p130 ()
        | 131 -> Redacted.p131 ()
        | 132 -> Redacted.p132 ()
        | 133 -> Redacted.p133 ()
        | 134 -> Redacted.p134 ()
        | 135 -> Redacted.p135 ()
        | 136 -> Redacted.p136 ()
        | 137 -> Redacted.p137 ()
        | 138 -> Redacted.p138 ()
        | 139 -> Redacted.p139 ()
        | 140 -> Redacted.p140 ()
        | 141 -> Redacted.p141 ()
        | 142 -> Redacted.p142 ()
        | 143 -> Redacted.p143 ()
        | 144 -> Redacted.p144 ()
        | 145 -> Redacted.p145 ()
        | 146 -> Redacted.p146 ()
        | 147 -> Redacted.p147 ()
        | 148 -> Redacted.p148 ()
        | 149 -> Redacted.p149 ()
        | 150 -> Redacted.p150 ()
        | 151 -> Redacted.p151 ()
        | 155 -> Redacted.p155 ()
        | 161 -> Redacted.p161 ()
        | 164 -> Redacted.p164 ()
        | 174 -> Redacted.p174 ()
        | 169 -> Redacted.p169 ()
        | 191 -> Redacted.p191 ()
        | 199 -> Redacted.p199 ()
        | 208 -> Redacted.p208 ()
        | 215 -> Redacted.p215 ()
        | 233 -> Redacted.p233 ()
        | 684 -> Redacted.p684 ()
        | 692 -> Redacted.p692 ()
        | 700 -> Redacted.p700 ()
        | 719 -> Redacted.p719 ()
        | 751 -> Redacted.p751 ()
        | _ -> "Problem either does not exist, or hasn't been solved yet"
    in match soln with
    | "" -> Printf.printf "\nExiting\n\n"
    | x -> Printf.printf "\t%s\n" x; main ()
  in main ()
