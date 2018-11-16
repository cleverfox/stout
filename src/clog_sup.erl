%%%-------------------------------------------------------------------
%% @doc clog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(clog_sup).

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
  {ok, { {one_for_all, 0, 1},
         [
          #{id=>console,
            start=>{clog_sink_console,
                    start_link,
                    [console, #{} ]
                   }
           },
          #{id=>sink1,
            start=>{clog_sink_file,start_link,[sink1, 
                                               #{filename=> "log/sink1.log"}
                                              ]}
           }
         ]
       } }.

%%====================================================================
%% Internal functions
%%====================================================================
