#!/usr/bin/env escript
%% https://www.spoj.com/problems/DIRVS/
%% Finds the shortest path between two BTS locations so that at least one of
%% them is always visible and the technician can only move to neighbouring
%% squares with limited height differences.

main(_) ->
    Data = read_all(),
    Tokens = [list_to_integer(T) || T <- string:tokens(Data, " \n\t\r")],
    solve(Tokens).

read_all() ->
    case io:get_line("") of
        eof -> "";
        Line -> Line ++ read_all()
    end.

solve([T|Rest]) ->
    solve_cases(T, Rest).

solve_cases(0, _) -> ok;
solve_cases(N, Tokens) ->
    [P,Q|Rest1] = Tokens,
    {Grid, Rest2} = read_grid(P, Q, Rest1),
    [R1,C1,R2,C2|Rest3] = Rest2,
    Res = bfs(Grid, P, Q, R1, C1, R2, C2),
    case Res of
        inf -> io:format("Mission impossible!~n");
        M -> io:format("The shortest path is ~B steps long.~n", [M])
    end,
    solve_cases(N-1, Rest3).

read_grid(0, _, Tokens) -> {[], Tokens};
read_grid(P, Q, Tokens) ->
    {Row, Rest1} = take(Q, Tokens),
    {Rows, Rest2} = read_grid(P-1, Q, Rest1),
    {[Row|Rows], Rest2}.

take(N, Tokens) -> take(N, Tokens, []).

take(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
take(N, [H|T], Acc) -> take(N-1, T, [H|Acc]).

get_h(Grid, R, C) ->
    Row = lists:nth(R, Grid),
    lists:nth(C, Row).

bfs(Grid, P, Q, R1, C1, R2, C2) ->
    Vis1 = compute_vis(Grid, P, Q, R1, C1),
    Vis2 = compute_vis(Grid, P, Q, R2, C2),
    Q0 = queue:from_list([{R1,C1,0}]),
    Visited0 = sets:add_element({R1,C1}, sets:new()),
    bfs_loop(Grid, P, Q, R2, C2, Vis1, Vis2, Q0, Visited0).

bfs_loop(Grid, P, Q, R2, C2, V1, V2, Queue, Visited) ->
    case queue:out(Queue) of
        {empty, _} -> inf;
        {{value, {R, C, D}}, Q1} ->
            if R =:= R2 andalso C =:= C2 ->
                    D;
               true ->
                    HR = get_h(Grid, R, C),
                    Neighs = [{R-1,C},{R+1,C},{R,C-1},{R,C+1}],
                    {Q2,Visited2} = lists:foldl(fun({NR,NC}, {Qacc,Vacc}) ->
                        if NR < 1; NR > P; NC < 1; NC > Q ->
                                {Qacc, Vacc};
                           true ->
                                case sets:is_element({NR,NC}, Vacc) of
                                    true -> {Qacc, Vacc};
                                    false ->
                                        HN = get_h(Grid, NR, NC),
                                        Diff = HN - HR,
                                        if Diff > 1; Diff < -3 -> {Qacc, Vacc};
                                           true ->
                                                Visible = maps:get({NR,NC}, V1, false) orelse maps:get({NR,NC}, V2, false),
                                                if Visible ->
                                                        {queue:in({NR,NC,D+1}, Qacc), sets:add_element({NR,NC}, Vacc)};
                                                   true -> {Qacc, Vacc}
                                                end
                                        end
                                end
                        end
                    end, {Q1,Visited}, Neighs),
                    bfs_loop(Grid, P, Q, R2, C2, V1, V2, Q2, Visited2)
            end
    end.

compute_vis(Grid, P, Q, BR, BC) ->
    lists:foldl(fun(R, Acc1) ->
        lists:foldl(fun(C, Acc) ->
            Vis = visible(Grid, P, Q, R, C, BR, BC),
            maps:put({R,C}, Vis, Acc)
        end, Acc1, lists:seq(1, Q))
    end, #{}, lists:seq(1, P)).

visible(Grid, P, Q, R, C, BR, BC) ->
    X1 = C - 0.5,
    Y1 = R - 0.5,
    Z1 = get_h(Grid, R, C) + 0.5,
    X2 = BC - 0.5,
    Y2 = BR - 0.5,
    Z2 = get_h(Grid, BR, BC) + 0.5,
    Dx = X2 - X1,
    Dy = Y2 - Y1,
    Dz = Z2 - Z1,
    Dist = math:sqrt(Dx*Dx + Dy*Dy + Dz*Dz),
    Steps = trunc(Dist * 20) + 1,
    StepT = 1.0 / Steps,
    visible_step(Grid, P, Q, X1, Y1, Z1, Dx, Dy, Dz, StepT, 1, Steps-1).

visible_step(_Grid, _P, _Q, _X1, _Y1, _Z1, _Dx, _Dy, _Dz, _StepT, I, Max) when I >= Max -> true;
visible_step(Grid, P, Q, X1, Y1, Z1, Dx, Dy, Dz, StepT, I, Max) ->
    T = I * StepT,
    X = X1 + Dx * T,
    Y = Y1 + Dy * T,
    Z = Z1 + Dz * T,
    RIdx = trunc(math:floor(Y) + 1),
    CIdx = trunc(math:floor(X) + 1),
    if RIdx < 1; RIdx > P; CIdx < 1; CIdx > Q -> false;
       true ->
           H = get_h(Grid, RIdx, CIdx),
           if Z =< H -> false;
              true -> visible_step(Grid, P, Q, X1, Y1, Z1, Dx, Dy, Dz, StepT, I+1, Max)
           end
    end.
