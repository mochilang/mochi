% csv-to-html-translation-5.erl - generated from csv-to-html-translation-5.mochi

split(S, Sep) ->
    Out0 = [],
    Start0 = 0,
    I0 = 0,
    N = length(Sep),
    (fun Loop0() -> case (I0 =< (length(S) - N)) of true -> (case (string:substr(S, (I0)+1, ((I0 + N))-(I0)) == Sep) of true -> Out1 = Out0 ++ [string:substr(S, (Start0)+1, (I0)-(Start0))], I1 = (I0 + N), Start1 = I1; _ -> I2 = (I1 + 1) end), Loop0(); _ -> ok end end()),
    Out2 = Out1 ++ [string:substr(S, (Start1)+1, (length(S))-(Start1))],
    Out2.

htmlEscape(S) ->
    Out3 = "",
    I3 = 0,
    (fun Loop1(I) -> case (I < length(S)) of true -> Ch = string:substr(S, (I)+1, ((I + 1))-(I)), (case (Ch == "&") of true -> Out4 = Out3 ++ "&amp;"; _ -> (case (Ch == "<") of true -> Out5 = Out4 ++ "&lt;"; _ -> (case (Ch == ">") of true -> Out6 = Out5 ++ "&gt;"; _ -> Out7 = (Out6 + Ch) end) end) end), I4 = (I + 1), Loop1(I4); _ -> ok end end(I3)),
    Out7.

main(_) ->
    C = "Character,Speech\n" ++ "The multitude,The messiah! Show us the messiah!\n" ++ "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" ++ "The multitude,Who are you?\n" ++ "Brians mother,I'm his mother; that's who!\n" ++ "The multitude,Behold his mother! Behold his mother!",
    Rows0 = [],
    {Rows1} = lists:foldl(fun(Line, {Rows}) -> Rows1 = Rows ++ [split(Line, ",")], {Rows1} end, {Rows0}, split(C, "\n")),
    io:format("~p~n", ["<table>"]),
    lists:foreach(fun(Row) -> Cells0 = "", {Cells1} = lists:foldl(fun(Cell, {Cells}) -> Cells1 = Cells ++ "<td>" ++ htmlEscape(Cell) ++ "</td>", {Cells1} end, {Cells0}, Row), io:format("~p~n", ["    <tr>" ++ Cells1 ++ "</tr>"]) end, Rows1),
    io:format("~p~n", ["</table>"]).
