%% mochi_agent_server: lightweight actor runtime for Mochi `spawn AgentType()`.
%% Phase 9.1 — message-loop-based agent process, no OTP gen_server dependency.
%%
%% Protocol:
%%   Caller → Agent : {call, CallerPid, Ref, IntentAtom, ArgList}
%%   Agent → Caller : {reply, Ref, Result}
%%   Caller → Agent : {cast, IntentAtom, ArgList}  (fire-and-forget)
%%
%% The DispatchFun/3 has signature:
%%   DispatchFun(IntentAtom :: atom(), Args :: list(), State :: map()) ->
%%     {Result :: term(), NewState :: map()}
%% For unit-returning intents, Result is the atom `ok`.
-module(mochi_agent_server).
-export([start/2, call/3, cast/3]).

%% start/2 — spawn a new agent process seeded with InitState.
%% Returns the PID (the opaque agent ref).
start(DispatchFun, InitState) ->
    spawn(fun() -> loop(DispatchFun, InitState) end).

%% loop/2 — internal receive loop; not exported.
loop(DispatchFun, State) ->
    receive
        {call, Caller, Ref, Intent, Args} ->
            {Result, NewState} = DispatchFun(Intent, Args, State),
            Caller ! {reply, Ref, Result},
            loop(DispatchFun, NewState);
        {cast, Intent, Args} ->
            {_Result, NewState} = DispatchFun(Intent, Args, State),
            loop(DispatchFun, NewState)
    end.

%% call/3 — synchronous call; blocks until the agent replies.
call(Pid, Intent, Args) ->
    Ref = make_ref(),
    Pid ! {call, self(), Ref, Intent, Args},
    receive
        {reply, Ref, Result} -> Result
    end.

%% cast/2 — asynchronous cast; returns immediately.
cast(Pid, Intent, Args) ->
    Pid ! {cast, Intent, Args},
    ok.
