-module(stout_format).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-17").

-include("include/stout_names.hrl").

-export([format/2,t2now/1,format_time/1,known_kinds/0,formats/0]).

t2now(T) ->
  USec=(T rem 1000000000) div 1000,
  RSec=T  div 1000000000,
  MegaSec=RSec div 1000000,
  Sec=RSec rem 1000000,
  Now={MegaSec, Sec, USec},
  {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
  {Date, {Hours, Minutes, Seconds, USec div 1000 rem 1000}}.

i2l(I) when I < 10  -> [$0, $0+I];
i2l(I)              -> integer_to_list(I).
i3l(I) when I < 100 -> [$0 | i2l(I)];
i3l(I)              -> integer_to_list(I).

format_time(T) ->
  {{Y, M, D}, {H, Mi, S, Ms}}=t2now(T),
  {[integer_to_list(Y), $-, i2l(M), $-, i2l(D)],
   [i2l(H), $:, i2l(Mi), $:, i2l(S), $., i3l(Ms)]}.

format(Kind, Args) ->
  try
  #{Kind:=Format}=?STOUT_FORMATS,
  lists:flatten(
    lists:map(
      fun(E) when is_list(E) -> E;
         (E) when is_atom(E) ->
          case lists:keyfind(E,1,Args) of
            {E, Val} when is_integer(Val) -> integer_to_list(Val);
            {E, Val} when E==time; E==date -> io_lib:format("~s",[Val]);
            {E, Val} -> io_lib:format("~p",[Val]);
            false -> "?"
          end;
         ({{M,F},E}) when is_atom(M), is_atom(F), is_atom(E) ->
          case {erlang:function_exported(M,F,1),
                lists:keyfind(E,1,Args)} of
            {_, false} -> "?";
            {true, {E,Val}} -> M:F(Val);
            {false, {E, Val}} when is_integer(Val) -> integer_to_list(Val);
            {false, {E, Val}} when E==time; E==date -> io_lib:format("~s",[Val]);
            {false, {E, Val}} -> io_lib:format("~p",[Val]);
            {false, false} -> "?"
          end
      end, Format)
   )
  catch error:{badmatch,_} ->
          {Kind,Args}  
  end.

known_kinds() ->
  ?STOUT_NAMES.
formats() ->
  ?STOUT_FORMATS.
