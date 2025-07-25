% bin-given-limits.erl - generated from bin-given-limits.mochi

getBins(Limits, Data) ->
    N0 = length(Limits),
    Bins0 = [],
    I0 = 0,
    (fun Loop0(Bins, I) -> case (I < (N0 + 1)) of true -> Bins1 = Bins ++ [0], I1 = (I + 1), Loop0(Bins1, I1); _ -> ok end end(Bins0, I0)),
    J0 = 0,
    (fun Loop2(Bins, J) -> case (J < length(Data)) of true -> D = lists:nth((J)+1, Data), Index0 = 0, (fun Loop1(Index) -> case (Index < length(Limits)) of true -> (case (D < lists:nth((Index)+1, Limits)) of true -> throw(break); _ -> ok end), (case (D == lists:nth((Index)+1, Limits)) of true -> Index1 = (Index + 1), throw(break); _ -> ok end), Index2 = (Index + 1), Loop1(Index2); _ -> ok end end(Index0)), Bins2 = lists:sublist(Bins, Index2) ++ [(lists:nth((Index2)+1, Bins) + 1)] ++ lists:nthtail((Index2)+1, Bins), J1 = (J + 1), Loop2(Bins2, J1); _ -> ok end end(Bins1, J0)),
    Bins2.

padLeft(N, Width) ->
    S0 = lists:flatten(io_lib:format("~p", [N0])),
    Pad0 = (Width - length(S0)),
    Out0 = "",
    I2 = 0,
    (fun Loop3(Out, I) -> case (I < Pad0) of true -> Out1 = Out ++ " ", I3 = (I + 1), Loop3(Out1, I3); _ -> ok end end(Out0, I2)),
    (Out1 + S0).

printBins(Limits, Bins) ->
    N1 = length(Limits),
    io:format("~p~n", ["           < " ++ padLeft(lists:nth((0)+1, Limits), 3) ++ " = " ++ padLeft(lists:nth((0)+1, Bins2), 2)]),
    I4 = 1,
    (fun Loop4(I) -> case (I < N1) of true -> io:format("~p~n", [">= " ++ padLeft(lists:nth(((I - 1))+1, Limits), 3) ++ " and < " ++ padLeft(lists:nth((I)+1, Limits), 3) ++ " = " ++ padLeft(lists:nth((I)+1, Bins2), 2)]), I5 = (I + 1), Loop4(I5); _ -> ok end end(I4)),
    io:format("~p~n", [">= " ++ padLeft(lists:nth(((N1 - 1))+1, Limits), 3) ++ "           = " ++ padLeft(lists:nth((N1)+1, Bins2), 2)]),
    io:format("~p~n", [""]).

main() ->
    LimitsList = [[23, 37, 43, 53, 67, 83], [14, 18, 249, 312, 389, 392, 513, 591, 634, 720]],
    DataList = [[95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55], [445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749]],
    I6 = 0,
    (fun Loop5(I) -> case (I < length(LimitsList)) of true -> io:format("~p~n", ["Example " ++ lists:flatten(io_lib:format("~p", [(I + 1)])) ++ "\n"]), Bins = getBins(lists:nth((I)+1, LimitsList), lists:nth((I)+1, DataList)), printBins(lists:nth((I)+1, LimitsList), Bins), I7 = (I + 1), Loop5(I7); _ -> ok end end(I6)).

main(_) ->
    main().
