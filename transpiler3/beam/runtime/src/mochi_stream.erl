-module(mochi_stream).
-export([make_stream/1, emit/2, subscribe/1, subscribe_limit/2, recv_sub/1]).

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
    Sub = spawn(fun() -> sub_loop([], infinity) end),
    Stream ! {subscribe, Sub},
    Sub.

%% subscribe_limit/2 — spawn a subscriber with backpressure: messages are
%% dropped when the buffer already holds Limit items (Phase 10.2).
subscribe_limit(Stream, Limit) ->
    Sub = spawn(fun() -> sub_loop([], Limit) end),
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
%% Limit is either `infinity` (no cap) or a positive integer (Phase 10.2).
sub_loop(Buffer, Limit) ->
    receive
        {stream_event, Val} ->
            case should_drop(Buffer, Limit) of
                true ->
                    %% Buffer full: drop incoming message (backpressure).
                    sub_loop(Buffer, Limit);
                false ->
                    sub_loop(Buffer ++ [Val], Limit)
            end;
        {recv, Caller} ->
            case Buffer of
                [H | T] ->
                    Caller ! {sub_value, H},
                    sub_loop(T, Limit);
                [] ->
                    receive
                        {stream_event, Val} ->
                            Caller ! {sub_value, Val},
                            sub_loop([], Limit)
                    end
            end
    end.

%% should_drop/2 — true when the buffer is at or beyond the limit.
should_drop(_Buffer, infinity) -> false;
should_drop(Buffer, Limit) -> length(Buffer) >= Limit.
