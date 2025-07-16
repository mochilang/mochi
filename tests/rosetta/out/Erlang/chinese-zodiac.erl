% chinese-zodiac.erl - generated from chinese-zodiac.mochi

cz(Yr, Animal, YinYang, Element, Sc, Bc) ->
    Y0 = (Yr - 4),
    Stem = rem(Y0, 10),
    Branch = rem(Y0, 12),
    Sb = (lists:nth((Stem)+1, Sc) + lists:nth((Branch)+1, Bc)),
    #{"__name" => "Info", animal => lists:nth((Branch)+1, Animal), yinYang => lists:nth((rem(Stem, 2))+1, YinYang), element => lists:nth((((Stem / 2)))+1, Element), stemBranch => Sb, cycle => (rem(Y0, 60) + 1)}.

main(_) ->
    Animal = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"],
    YinYang = ["Yang", "Yin"],
    Element = ["Wood", "Fire", "Earth", "Metal", "Water"],
    StemChArr = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"],
    BranchChArr = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"],
    lists:foreach(fun(Yr) -> R = cz(Yr, Animal, YinYang, Element, StemChArr, BranchChArr), io:format("~p~n", [lists:flatten(io_lib:format("~p", [Yr])) ++ ": " ++ mochi_get(element, R) ++ " " ++ mochi_get(animal, R) ++ ", " ++ mochi_get(yinYang, R) ++ ", Cycle year " ++ lists:flatten(io_lib:format("~p", [mochi_get(cycle, R)])) ++ " " ++ mochi_get(stemBranch, R)]) end, [1935, 1938, 1968, 1972, 1976]).

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
