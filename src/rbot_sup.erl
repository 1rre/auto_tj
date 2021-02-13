-module(rbot_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(rbot_sup, []).

init(_) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [#{id => rbot,
                  start => {rbot, start_loop, []},
                  restart => permanent,
                  type => worker,
                  modules => [rbot]}],
  {ok, {SupFlags, ChildSpecs}}.

