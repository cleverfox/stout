-module(stout_sink_console).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("include/stout_names.hrl").

-define(DEFAULT_SYNC_INTERVAL, 1000).
-define(DEFAULT_SYNC_SIZE, 1024*64). %% 64kb

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name,#{}=Opts) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts#{name=>Name}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#{name:=Name}=Args) ->
  T=erlang:send_after(1000,self(),heartbeat),
  ets:insert(stout_alive,{Name,erlang:system_time()}),
  {ok, Args#{htimer=>T} }.

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p",[_Request]),
  {reply, unhandled, State}.

handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p",[_Msg]),
  {noreply, State}.

handle_info(heartbeat, #{htimer:=T,name:=Name}=State) ->
  erlang:cancel_timer(T),
  ets:insert(stout_alive,{Name,erlang:system_time()}),
  T1=erlang:send_after(2000,self(),heartbeat),
  {noreply, State#{htimer=>T1}};

handle_info(rotate, State) ->
  {noreply, State};

handle_info({log, Timestamp, Kind, PropList}, State) when
    is_integer(Timestamp),
    is_atom(Kind),
    is_list(PropList)->
  Res=stout_format:format(Kind,PropList),
  if is_list(Res) ->
       erlang:display_string(Res);
     true ->
       erlang:display()
  end,
  {noreply, State};

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

