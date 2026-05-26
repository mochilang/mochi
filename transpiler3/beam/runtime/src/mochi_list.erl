-module(mochi_list).
-export([set/3]).

%% set/3: O(n) in-place element update for 0-indexed Mochi list mutation.
%% Equivalent to xs[i] = v in Mochi.
set([_|T], 0, V) -> [V|T];
set([H|T], I, V) when I > 0 -> [H | set(T, I - 1, V)].
