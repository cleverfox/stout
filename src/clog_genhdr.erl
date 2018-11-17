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
        lists:foldl(
          fun({K,Fmt}, A) ->
              [{K,Fmt,[]}|A];
             ({K,Fmt,Opts}, A) ->
              [{K,Fmt,Opts}|A]
          end, Acc, F)
    end, [], SearchPaths
   ),
  MsgNames=[ N || {N,_,_} <- Messages ],
  MsgFmt=[ {N,F} || {N,F,_} <- Messages ],
  MsgOpt=[ {N,O} || {N,_,O} <- Messages ],
  ok = filelib:ensure_dir("include/"),
  file:write_file("include/clog_names.hrl",
                  [
                  io_lib:format("-define(CLOG_NAMES,~p).~n", [MsgNames]),
                  io_lib:format("-define(CLOG_FORMATS,~p).~n", [maps:from_list(MsgFmt)]),
                  io_lib:format("-define(CLOG_OPTS,~p).~n", [maps:from_list(MsgOpt)])
                  ]
                 ).


