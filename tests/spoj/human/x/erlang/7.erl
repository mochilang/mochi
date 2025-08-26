#!/usr/bin/env escript
%% https://www.spoj.com/problems/BULK/
%% Given faces of a bulk built from unit cubes, compute the number of cubes.

main(_) ->
    T = read_int(),
    solve(T).

read_int() ->
    {ok, [N]} = io:fread("", "~d"),
    N.

solve(0) -> ok;
solve(T) ->
    F = read_int(),
    Xs0 = #{0 => true, 1001 => true},
    Ys0 = #{0 => true, 1001 => true},
    Zs0 = #{0 => true, 1001 => true},
    {Faces, Xs, Ys, Zs} = read_faces(F, [], Xs0, Ys0, Zs0),
    Xvals = lists:sort(maps:keys(Xs)),
    Yvals = lists:sort(maps:keys(Ys)),
    Zvals = lists:sort(maps:keys(Zs)),
    Vol = compute(Xvals, Yvals, Zvals, Faces),
    io:format("The bulk is composed of ~B units.~n", [Vol]),
    solve(T - 1).

read_faces(0, Faces, Xs, Ys, Zs) -> {Faces, Xs, Ys, Zs};
read_faces(F, Faces, Xs, Ys, Zs) ->
    P = read_int(),
    {Pts, Xs1, Ys1, Zs1} = read_points(P, Xs, Ys, Zs, []),
    Faces1 = case all_equal(Pts) of
                 true -> [{element(1, hd(Pts)), yz_poly(Pts)} | Faces];
                 false -> Faces
             end,
    read_faces(F - 1, Faces1, Xs1, Ys1, Zs1).

read_points(0, Xs, Ys, Zs, Acc) -> {lists:reverse(Acc), Xs, Ys, Zs};
read_points(P, Xs, Ys, Zs, Acc) ->
    X = read_int(),
    Y = read_int(),
    Z = read_int(),
    Xs1 = maps:put(X, true, Xs),
    Ys1 = maps:put(Y, true, Ys),
    Zs1 = maps:put(Z, true, Zs),
    read_points(P - 1, Xs1, Ys1, Zs1, [{X, Y, Z} | Acc]).

all_equal([]) -> true;
all_equal([{X, _, _} | Rest]) ->
    lists:all(fun({X1, _, _}) -> X1 =:= X end, Rest).

yz_poly(Pts) ->
    [ {Y, Z} || {_X, Y, Z} <- Pts ].

compute(Xs, Ys, Zs, Faces) -> compute_x(Xs, Ys, Zs, Faces, 0).

compute_x([_], _Ys, _Zs, _Faces, Acc) -> Acc;
compute_x([X1, X2 | XsRest], Ys, Zs, Faces, Acc) ->
    Xmid = (X1 + X2) / 2,
    Dx = X2 - X1,
    Acc1 = compute_y(Ys, Zs, Faces, Xmid, Dx, Acc),
    compute_x([X2 | XsRest], Ys, Zs, Faces, Acc1).

compute_y([_], _Zs, _Faces, _Xmid, _Dx, Acc) -> Acc;
compute_y([Y1, Y2 | YsRest], Zs, Faces, Xmid, Dx, Acc) ->
    Ymid = (Y1 + Y2) / 2,
    Dy = Y2 - Y1,
    Acc1 = compute_z(Zs, Faces, Xmid, Ymid, Dx, Dy, Acc),
    compute_y([Y2 | YsRest], Zs, Faces, Xmid, Dx, Acc1).

compute_z([_], _Faces, _Xmid, _Ymid, _Dx, _Dy, Acc) -> Acc;
compute_z([Z1, Z2 | ZsRest], Faces, Xmid, Ymid, Dx, Dy, Acc) ->
    Zmid = (Z1 + Z2) / 2,
    Dz = Z2 - Z1,
    Count = count_faces(Faces, Xmid, Ymid, Zmid),
    Acc1 = case Count rem 2 of
               1 -> Acc + Dx * Dy * Dz;
               _ -> Acc
           end,
    compute_z([Z2 | ZsRest], Faces, Xmid, Ymid, Dx, Dy, Acc1).

count_faces(Faces, Xmid, Ymid, Zmid) ->
    lists:foldl(
      fun({Coord, Poly}, C) ->
          case Xmid < Coord andalso point_in_poly(Poly, Ymid, Zmid) of
              true -> C + 1;
              false -> C
          end
      end, 0, Faces).

point_in_poly(Poly, Y, Z) ->
    point_in_poly(Poly, Y, Z, lists:last(Poly), false).

point_in_poly([], _Y, _Z, _Last, Inside) -> Inside;
point_in_poly([{Yi, Zi} | Rest], Y, Z, {Yj, Zj}, Inside) ->
    Cond = ((Zi > Z) /= (Zj > Z)) andalso
           (Y < (Yj - Yi) * (Z - Zi) / (Zj - Zi) + Yi),
    Inside1 = if Cond -> not Inside; true -> Inside end,
    point_in_poly(Rest, Y, Z, {Yi, Zi}, Inside1).
