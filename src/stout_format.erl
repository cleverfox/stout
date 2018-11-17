-module(stout_format).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-17").

-include("include/stout_names.hrl").

-export([format/2]).

format(Kind, Args) ->
  try
  #{Kind:=Format}=?STOUT_FORMATS,
  lists:flatten(
    lists:map(
      fun(E) when is_list(E) -> E;
         (E) when is_atom(E) ->
          case lists:keyfind(E,1,Args) of
            {E, Val} when is_integer(Val) -> integer_to_list(Val);
            {E, Val} -> io_lib:format("~p",[Val]);
            false -> "?"
          end
      end, Format)
   )
  catch error:{badmatch,_} ->
          {Kind,Args}  
  end.

