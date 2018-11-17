-module(clog_genhdr).
-export([main/1]).

main(_) ->
  lognames().

lognames() ->
  %filelib:wildcard("apps/**/*.{erl,hrl}").
  SearchPaths=filelib:wildcard("apps/**/*.clogmsg")
  ++ filelib:wildcard("src/**/*.clogmsg"),
  Messages=lists:foldl(
    fun(Filename, Acc) ->
        {ok, F} = file:consult(Filename),
        F++Acc
    end, [], SearchPaths
   ),
  MsgNames=[ N || {N,_} <- Messages ],
  ok = filelib:ensure_dir("include/"),
  file:write_file("include/clog_names.hrl",
                  io_lib:format("-define(CLOG_NAMES,~p).~n", [MsgNames])
                 ).


