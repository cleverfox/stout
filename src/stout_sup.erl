%%%-------------------------------------------------------------------
%% @doc stout top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(stout_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { {one_for_one, 5, 10},
         [
          #{id=>stout_conf, start=>{ stout_conf, start_link, [] } }
         ]
       } }.

%%====================================================================
%% Internal functions
%%====================================================================
