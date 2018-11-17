-module(clog_conf).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-15").

-include("include/clog_names.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-behaviour(gen_server).
-define(TABLE, stout_routes).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,read_config/0,rt_table/1]).

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
  self() ! reload,
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p",[_Request]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p",[_Msg]),
  {noreply, State}.

handle_info(reload, State) ->
  Config=read_config(),
  RT=maps:to_list(rt_table(Config)),
  ets:insert(?TABLE,RT),
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

%filelib:wildcard("apps/**/*.{erl,hrl}").

fix_route({Kind, Dst}) when is_atom(Dst) ->
  fix_route({Kind, Dst, []});

fix_route({any, Dst, Lst}) when is_atom(Dst), is_list(Lst) ->
  {?CLOG_NAMES, Dst, Lst};

fix_route({Kind, Dst, Lst}) when is_atom(Dst), is_list(Lst),
                                 is_atom(Kind) ->
  fix_route({[Kind], Dst, Lst});

fix_route({Kind, Dst, Lst}) when is_atom(Dst), is_list(Lst),
                                 is_list(Kind) ->
  {Kind, Dst, Lst}.

rt_table(Config) ->
  Routes0=proplists:get_value(routing, Config, []),
  Routes=lists:map(fun fix_route/1, Routes0),
  lists:foldl(
    fun({Messages, Sink, Filter},Acc) ->
        Dst=case Filter of
              [] -> Sink;
              [{_,_}|_] ->
                {
                 compile_filter(Filter),
                 Sink
                }
            end,
        lists:foldl(
          fun(Msg, Acc1) ->
              L0=maps:get(Msg, Acc1, []),
              maps:put(Msg,[Dst|L0],Acc1)
          end, Acc, Messages)
    end, #{}, Routes).

compile_filter(Filter) ->
  FunSrc=lists:flatten("fun(Params) -> "++compile_filter_clause(Filter)++" end."),
  {ok, Tokens, _} = erl_scan:string(FunSrc),
  {ok, Parsed} = erl_parse:parse_exprs(Tokens),
  {value, Result, _} = erl_eval:exprs(Parsed, erl_eval:new_bindings()),
  Result.

%merl:show(merl:quote("fun(Params) -> case lists:keyfind(module,1,Params) of blockchain -> case lists:keyfind(username,1,Params) of \"ivan\" -> true; _ -> false end; _ -> false end end")).
%
compile_filter_clause([]) ->
  "true";

compile_filter_clause([{Key,Value}|Rest]) ->
  [
   io_lib:format("case lists:keyfind(~s,1,Params) of {~s,~p} -> ",[Key,Key,Value]),
   compile_filter_clause(Rest), ";",
   " _ -> false end"].

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
     {any, debug, [{module, blockchain},{username, "ivan"}]},
     {any, debug1, [{module, blockchain},{username, "debug1"}]},
     {[accept_block,mkblock_debug], sink1},
     {[test1], console, [{var1,1}]}
    ]
   }
  ].

