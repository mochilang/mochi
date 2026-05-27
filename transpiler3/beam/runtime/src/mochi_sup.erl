-module(mochi_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% init/1 — top-level supervisor.
%% Phase 9.4: starts mochi_agent_sup as a permanent child so all spawned
%% agents run under OTP supervision with transient restart policy.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    AgentSupSpec = #{
        id       => mochi_agent_sup,
        start    => {mochi_agent_sup, start_link, []},
        restart  => permanent,
        shutdown => infinity,
        type     => supervisor,
        modules  => [mochi_agent_sup]
    },
    {ok, {SupFlags, [AgentSupSpec]}}.
