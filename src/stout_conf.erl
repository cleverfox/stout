-module(stout_conf).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-15").

-include("include/stout_names.hrl").

-behaviour(gen_server).
-define(TABLE, stout_routes).
-define(TABLES, stout_alive).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,read_config/0,rt_table/1,spawn_sinks/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  ?TABLE=ets:new(?TABLE,[named_table,protected,set,{read_concurrency,true}]),
  ?TABLES=ets:new(?TABLES,[named_table,public,set,{read_concurrency,true}]),
  self() ! reload,
  {ok, #{pre_wrk=>#{}}}.

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p",[_Request]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p",[_Msg]),
  {noreply, State}.

handle_info(reload, #{pre_wrk:=Pre}=State) ->
  Config=read_config(),
  Workers=spawn_sinks(Config),
  Post=lists:foldl(
    fun(#{id:=ID}=Wrk,Acc) ->
        Old=maps:get(ID,Pre,undefined),
        if(Wrk==Old) ->
            maps:put(ID,Wrk,Acc);
          true ->
            if(Old==undefined) ->
                supervisor:start_child(stout_sup,Wrk),
                maps:put(ID,Wrk,Acc);
              true ->
                supervisor:terminate_child(stout_sup,ID),
                supervisor:delete_child(stout_sup,ID),
                supervisor:start_child(stout_sup,Wrk),
                maps:put(ID,Wrk,Acc)
            end
        end
    end, #{}, Workers),
  Delete=maps:keys(Pre)--maps:keys(Post),
  lists:foreach(fun(ID) ->
                    supervisor:terminate_child(stout_sup,ID),
                    supervisor:delete_child(stout_sup,ID)
                end, Delete),
  RT=rt_table(Config),
  ets:insert(?TABLE,RT),
  {noreply, State#{pre_wrk=>Post}};

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

fix_route({Kind, Dst}) when is_atom(Dst) ->
  fix_route({Kind, Dst, []});

fix_route({any, Dst, Lst}) when is_atom(Dst), is_list(Lst) ->
  {?STOUT_NAMES, Dst, Lst};

fix_route({Kind, Dst, Lst}) when is_atom(Dst), is_list(Lst),
                                 is_atom(Kind) ->
  fix_route({[Kind], Dst, Lst});

fix_route({Kind, Dst, Lst}) when is_atom(Dst), is_list(Lst),
                                 is_list(Kind) ->
  {Kind, Dst, Lst}.

spawn_sinks(Config) ->
  Sinks=proplists:get_value(sinks, Config, []),
  lists:map(
    fun({ID, Type, Opts}) ->
        #{id=>atom2log(ID), start=>{Type, start_link, [atom2log(ID), Opts ] } }
    end,
    Sinks).

rt_table(Config) ->
  Routes0=proplists:get_value(routing, Config, []),
  Routes=lists:map(fun fix_route/1, Routes0),
  Map=lists:foldl(
    fun({Messages, Sink, Filter},Acc) ->
        Dst=case Filter of
              [] -> atom2log(Sink);
              [{_,_}|_] ->
                {
                 compile_filter(Filter),
                 atom2log(Sink)
                }
            end,
        lists:foldl(
          fun(Msg, Acc1) ->
              L0=maps:get(Msg, Acc1, []),
              maps:put(Msg,[Dst|L0],Acc1)
          end, Acc, Messages)
    end, #{}, Routes),
  io:format("Map ~p",[Map]),
  [ {K,V, maps:get(K,?STOUT_OPTS,[])} || {K,V} <- maps:to_list(Map) ].

compile_filter(Filter) ->
  FunSrc=lists:flatten("fun(Params) -> "++compile_filter_clause(Filter)++" end."),
  {ok, Tokens, _} = erl_scan:string(FunSrc),
  {ok, Parsed} = erl_parse:parse_exprs(Tokens),
  {value, Result, _} = erl_eval:exprs(Parsed, erl_eval:new_bindings()),
  Result.

compile_filter_clause([]) ->
  "true";

compile_filter_clause([{Key,Value}|Rest]) ->
  [
   io_lib:format("case lists:keyfind(~s,1,Params) of {~s,~p} -> ",
                 [Key,Key,Value]),
   compile_filter_clause(Rest), ";",
   " _ -> false end"].

read_config() ->
  Filename=application:get_env(stout,configfile,"stout.conf"),
  Config=case file:consult(Filename) of
           {ok, Cfg} ->
             Cfg;
           {error, enoent} ->
             default_config()
         end,
  Config.

atom2log(Atom) ->
  list_to_atom("stout_"++atom_to_list(Atom)).

default_config() ->
  [
   {sinks, 
    [
     {console, stout_sink_console, #{}},
     {sink1, stout_sink_file, #{filename=>"log/sink1.log"}},
     {debug, stout_sink_file, #{filename=>"log/sdebug.log"}}
    ]
   },
   {routing, 
    [
     {any, console, []},
     {any, debug, []}
     %{[accept_block,mkblock_debug], sink1},
     %{[test1], console, [{var1,1}]}
    ]
   }
  ].

