%% mochi_agent_server: lightweight actor runtime for Mochi `spawn AgentType()`.
%% Phase 9.1 — message-loop-based agent process, no OTP gen_server dependency.
%% Phase 9.3 — optional terminate callback invoked on process shutdown.
%% Phase 9.4 — start_link/3 for use under mochi_agent_sup dynamic supervisor.
%%
%% Protocol:
%%   Caller → Agent : {call, CallerPid, Ref, IntentAtom, ArgList}
%%   Agent → Caller : {reply, Ref, Result}
%%   Caller → Agent : {cast, IntentAtom, ArgList}  (fire-and-forget)
%%   Caller → Agent : stop  (graceful shutdown; invokes terminate if provided)
%%
%% The DispatchFun/3 has signature:
%%   DispatchFun(IntentAtom :: atom(), Args :: list(), State :: map()) ->
%%     {Result :: term(), NewState :: map()}
%% For unit-returning intents, Result is the atom `ok`.
%%
%% The optional TerminateFun/1 has signature:
%%   TerminateFun(FinalState :: map()) -> ok
%% Called when the process receives `stop` or exits normally.
-module(mochi_agent_server).
-export([start/2, start/3, start_link/3, call/3, cast/3, stop/1]).

%% start/2 — spawn a new agent process seeded with InitState (no terminate callback).
%% Returns the PID (the opaque agent ref).
start(DispatchFun, InitState) ->
    start(DispatchFun, InitState, none).

%% start/3 — spawn a new agent process with an optional TerminateFun.
%% TerminateFun is either a fun/1 or the atom `none`.
start(DispatchFun, InitState, TerminateFun) ->
    spawn(fun() -> loop(DispatchFun, TerminateFun, InitState) end).

%% start_link/3 — OTP-compatible entry point for use under a supervisor.
%% Links the caller (supervisor) to the agent process for crash detection.
start_link(DispatchFun, InitState, TerminateFun) ->
    {ok, spawn_link(fun() -> loop(DispatchFun, TerminateFun, InitState) end)}.

%% loop/3 — internal receive loop; not exported.
loop(DispatchFun, TerminateFun, State) ->
    receive
        {call, Caller, Ref, Intent, Args} ->
            {Result, NewState} = DispatchFun(Intent, Args, State),
            Caller ! {reply, Ref, Result},
            loop(DispatchFun, TerminateFun, NewState);
        {cast, Intent, Args} ->
            {_Result, NewState} = DispatchFun(Intent, Args, State),
            loop(DispatchFun, TerminateFun, NewState);
        stop ->
            run_terminate(TerminateFun, State)
    end.

%% run_terminate/2 — call TerminateFun if present, then exit normally.
run_terminate(none, _State) ->
    ok;
run_terminate(TerminateFun, State) ->
    TerminateFun(State),
    ok.

%% call/3 — synchronous call; blocks until the agent replies.
call(Pid, Intent, Args) ->
    Ref = make_ref(),
    Pid ! {call, self(), Ref, Intent, Args},
    receive
        {reply, Ref, Result} -> Result
    end.

%% cast/3 — asynchronous cast; returns immediately.
cast(Pid, Intent, Args) ->
    Pid ! {cast, Intent, Args},
    ok.

%% stop/1 — send a graceful stop signal; triggers terminate callback if present.
stop(Pid) ->
    Pid ! stop,
    ok.
