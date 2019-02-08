-module(stout_genhdr).
-export([main/1]).

main(_) ->
  io:format("generating stout headers...~n"),
  lognames(),
  io:format("done.~n").

lognames() ->
  Root = os:getenv("REBAR_ROOT_DIR", "."),
  
  %filelib:wildcard(Root ++ "/apps/**/*.{erl,hrl}").
  SearchPaths =
    filelib:wildcard(Root ++ "/apps/**/*.stout")
    ++ filelib:wildcard(Root ++ "/src/**/*.stout"),
  
  Messages = lists:foldl(
    fun(Filename, Acc) ->
      {ok, F} = file:consult(Filename),
      lists:foldl(
        fun({K, Fmt}, A) ->
          [{K, Fmt, []} | A];
          ({K, Fmt, Opts}, A) ->
            [{K, Fmt, Opts} | A]
        end, Acc, F)
    end, [], SearchPaths
  ),
  
  MsgNames = [N || {N, _, _} <- Messages],
  MsgFmt = [{N, F} || {N, F, _} <- Messages],
  MsgOpt = [{N, O} || {N, _, O} <- Messages],
  
  ok = filelib:ensure_dir(Root ++ "/include/"),
  
  file:write_file(
    Root ++ "/include/stout_names.hrl",
    [
      io_lib:format("-define(STOUT_NAMES,~p).~n", [MsgNames]),
      io_lib:format("-define(STOUT_FORMATS,~p).~n", [maps:from_list(MsgFmt)]),
      io_lib:format("-define(STOUT_OPTS,~p).~n", [maps:from_list(MsgOpt)])
    ]
  ).
