%% mochi_agent_sup: dynamic supervisor for Mochi spawned agents.
%% Phase 9.4 — wraps mochi_agent_server processes with transient restart policy.
%%
%% Transient policy: the supervisor restarts a child only if it exits
%% abnormally (crash). Normal exits (e.g. receiving `stop`) are not restarted.
%%
%% Usage:
%%   mochi_agent_sup:start_link()   % start the supervisor (called by mochi_app)
%%   mochi_agent_sup:start_agent(DispatchFun, InitState)
%%   mochi_agent_sup:start_agent(DispatchFun, InitState, TerminateFun)
-module(mochi_agent_sup).
-behaviour(supervisor).
-export([start_link/0, start_agent/2, start_agent/3]).
-export([init/1]).

%% start_link/0 — start the dynamic supervisor, registered locally.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% init/1 — OTP supervisor callback. Returns the supervisor spec.
%% We use a simple_one_for_one strategy so we can dynamically add children.
init([]) ->
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 10,
        period    => 60
    },
    %% Child spec template: each child is a mochi_agent_server process.
    %% restart => transient means only abnormal exits trigger restart.
    ChildSpec = #{
        id       => mochi_agent_worker,
        start    => {mochi_agent_server, start_link, []},
        restart  => transient,
        shutdown => 5000,
        type     => worker,
        modules  => [mochi_agent_server]
    },
    {ok, {SupFlags, [ChildSpec]}}.

%% start_agent/2 — dynamically add an agent child (no terminate callback).
start_agent(DispatchFun, InitState) ->
    supervisor:start_child(?MODULE, [DispatchFun, InitState, none]).

%% start_agent/3 — dynamically add an agent child with terminate callback.
start_agent(DispatchFun, InitState, TerminateFun) ->
    supervisor:start_child(?MODULE, [DispatchFun, InitState, TerminateFun]).
