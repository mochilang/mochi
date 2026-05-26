-module(mochi_chan).
-export([make_chan/1, send/2, recv/1]).

%% make_chan/1 — spawn a buffered FIFO channel process and return its PID.
make_chan(_Cap) ->
    spawn(fun() -> chan_loop([]) end).

%% send/2 — enqueue Val into the channel.
send(Chan, Val) ->
    Chan ! {send, Val},
    ok.

%% recv/1 — dequeue the next value from the channel (blocks if empty).
recv(Chan) ->
    Chan ! {recv, self()},
    receive
        {chan_value, Val} -> Val
    end.

%% Internal: channel FIFO loop.
chan_loop(Queue) ->
    receive
        {send, Val} ->
            chan_loop(Queue ++ [Val]);
        {recv, Caller} ->
            case Queue of
                [H | T] ->
                    Caller ! {chan_value, H},
                    chan_loop(T);
                [] ->
                    receive
                        {send, Val} ->
                            Caller ! {chan_value, Val},
                            chan_loop([])
                    end
            end
    end.
