% csv-to-html-translation-1.erl - generated from csv-to-html-translation-1.mochi

main(_) ->
    C = "Character,Speech\n" ++ "The multitude,The messiah! Show us the messiah!\n" ++ "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" ++ "The multitude,Who are you?\n" ++ "Brians mother,I'm his mother; that's who!\n" ++ "The multitude,Behold his mother! Behold his mother!",
    Rows0 = [],
    {Rows1} = lists:foldl(fun(Line, {Rows}) -> Rows1 = Rows ++ [split(Line, ",")], {Rows1} end, {Rows0}, split(C, "\n")),
    io:format("~p~n", ["<table>"]),
    lists:foreach(fun(Row) -> Cells0 = "", {Cells1} = lists:foldl(fun(Cell, {Cells}) -> Cells1 = Cells ++ "<td>" ++ Cell ++ "</td>", {Cells1} end, {Cells0}, Row), io:format("~p~n", ["    <tr>" ++ Cells1 ++ "</tr>"]) end, Rows1),
    io:format("~p~n", ["</table>"]).
