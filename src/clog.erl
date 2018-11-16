-module(clog).
-export([start/0,stop/0,log/5,log/2,log_to/3,md/1]).

start() ->
  application:start(clog).

stop() ->
  application:stop(clog).

md(Proplist) ->
  Pre=case get(clog_md) of
        undefined -> [];
        L when is_list(L) -> L
      end,
  New=lists:foldl(
    fun({K,V},Acc) ->
        [{K,V}|lists:keydelete(K,1,Acc)]
    end, Pre, Proplist),
  put(clog_md,New).

log_to(Sink, Kind,Data) when is_atom(Kind), is_list(Data) ->
  T=os:system_time(),
  MD=case get(clog_md) of
       undefined -> Data;
       L when is_list(L) -> Data++L
     end,
  try
    Sink ! {log, T, Kind, MD},
    ok
  catch _:_ -> error
  end.

log(Kind, Data, File, Line, Stack) ->
  log(Kind, [{at_filename,File},{at_line,Line},{at_stack,Stack}|Data]).

log(Kind,Data) when is_atom(Kind),
                    is_list(Data) ->
  T=os:system_time(),
  MD=case get(clog_md) of
       undefined -> Data;
       L when is_list(L) -> Data++L 
     end,
  try
    sink1 ! {log, T, Kind, MD},
    ok
  catch _:_ -> error
  end.

