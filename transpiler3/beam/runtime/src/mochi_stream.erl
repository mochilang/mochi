-module(mochi_stream).
-export([make_stream/1, emit/2, subscribe/1, recv_sub/1]).

%% make_stream/1 — spawn a stream broker process and return its PID.
%% Cap is ignored (BEAM mailboxes are unbounded).
make_stream(_Cap) ->
    spawn(fun() -> stream_loop([]) end).

%% emit/2 — broadcast Val to all current subscribers.
emit(Stream, Val) ->
    Stream ! {emit, Val},
    ok.

%% subscribe/1 — spawn a subscriber process registered with Stream.
subscribe(Stream) ->
    Sub = spawn(fun() -> sub_loop([]) end),
    Stream ! {subscribe, Sub},
    Sub.

%% recv_sub/1 — receive the next buffered value from a subscriber process.
recv_sub(Sub) ->
    Sub ! {recv, self()},
    receive
        {sub_value, Val} -> Val
    end.

%% Internal: stream broker loop.
stream_loop(Subs) ->
    receive
        {subscribe, SubPid} ->
            stream_loop([SubPid | Subs]);
        {emit, Val} ->
            lists:foreach(fun(S) -> S ! {stream_event, Val} end, Subs),
            stream_loop(Subs)
    end.

%% Internal: subscriber buffer loop.
sub_loop(Buffer) ->
    receive
        {stream_event, Val} ->
            sub_loop(Buffer ++ [Val]);
        {recv, Caller} ->
            case Buffer of
                [H | T] ->
                    Caller ! {sub_value, H},
                    sub_loop(T);
                [] ->
                    receive
                        {stream_event, Val} ->
                            Caller ! {sub_value, Val},
                            sub_loop([])
                    end
            end
    end.
