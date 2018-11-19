-module(stout).
-export([start/0,stop/0,log/5,log/4,log/2,log_to/3,md/1]).

start() ->
  application:start(stout).

stop() ->
  application:stop(stout).

md(Proplist) ->
  Pre=case get(stout_md) of
        undefined -> [];
        L when is_list(L) -> L
      end,
  New=lists:foldl(
    fun({K,V},Acc) ->
        [{K,V}|lists:keydelete(K,1,Acc)]
    end, Pre, Proplist),
  put(stout_md,New).

log_to(Sink, Kind,Data) when is_atom(Kind), is_list(Data) ->
  T=os:system_time(),
  MD=case get(stout_md) of
       undefined -> Data;
       L when is_list(L) -> Data++L
     end,
  try
    Sink ! {log, T, Kind, MD},
    ok
  catch _:_ -> error
  end.

log(Kind, Data, File, Line, Stack) ->
  log(Kind, [{module,File},{line,Line},{stacktrace,Stack}|Data]).

log(Kind, Data, File, Line) ->
  log(Kind, [{module,File},{line,Line}|Data]).

log(Kind,Data) when is_atom(Kind),
                    is_list(Data) ->
  %io:format("~s Args ~p~n",[Kind,Data]),
  T=erlang:system_time(),
  MD=case get(stout_md) of
       undefined -> Data;
       L when is_list(L) -> Data++L 
     end,
  try
    case ets:lookup(stout_routes, Kind) of
      [] ->
        ignore;
      [{Kind, Destinations,Opts}] ->
        Sent=lists:foldl(
               fun(Dst,Acc) when is_atom(Dst) ->
                   do_log_to(Dst, T, Kind, MD, Opts),
                   Acc+1;
                  ({Fun, Dst},Acc) when is_function(Fun),
                                        is_atom(Dst) ->
                   case Fun(MD) of
                     true ->
                       case do_log_to(Dst, T, Kind, MD, Opts) of
                         ok -> Acc+1;
                         error -> Acc
                       end;
                     false ->
                       Acc
                   end
               end, 0, Destinations),
        if Sent==0 -> none;
           true -> ok
        end
    end
  catch error:badarg ->
          no_running
  end.

do_log_to(Sink, T, Kind, MD, Opts) ->
  try
    case lists:member(critical, Opts) of
      true ->
         Sink ! {log, T, Kind, MD},
         ok;
      false ->
        [{Sink, ST}]=ets:lookup(stout_alive,Sink),
        if ST-T<5000 ->
             Sink ! {log, T, Kind, MD},
             ok;
           true ->
             overload
        end
    end
  catch _:_ -> error
  end.

