-module(clog_conf).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-15").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,read_config/0,msgfile/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    lager:notice("Unknown call ~p",[_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:notice("Unknown cast ~p",[_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:notice("Unknown info  ~p",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%filelib:wildcard("apps/**/*.{erl,hrl}").

read_config() ->
  Filename=application:get_env(clog,file,"clog.conf"),
  Config=case file:consult(Filename) of
           {ok, Cfg} ->
             Cfg;
           {error, enoent} ->
             default_config()
             
         end,
  Config.

default_config() ->
  [
   {sinks, 
    [
     {console, clog_sink_console, #{}},
     {sink1, clog_sink_file, #{filename=>"log/sink1.clog"}},
     {debug, clog_sink_file, #{filename=>"log/debug.clog"}}
    ]
   },
   {routing, 
    [
     {any, debug},
     {[test1], console}
    ]
   }
  ].

msgfile() ->
  %filelib:wildcard("apps/**/*.{erl,hrl}").
  SearchPaths=filelib:wildcard("apps/**/*.clogmsg")
  ++ filelib:wildcard("src/**/*.clogmsg"),
  lists:foldl(
    fun(Filename, Acc) ->
        {ok, F} = file:consult(Filename),
        F++Acc
    end, [], SearchPaths
   ).

