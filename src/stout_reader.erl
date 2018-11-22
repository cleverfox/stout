-module(stout_reader).
-export([fold/3, mfold/3]).

fold(Pred, Init, Filename) ->
  Options=[raw, read, binary, {read_ahead, 1024}],
  case file:open(Filename, Options) of
    {ok, FD} ->
      Res=do_fold(Pred, Init, FD),
      file:close(FD),
      Res;
    Any ->
      Any
  end.

do_fold(Pred, Acc, FD) ->
  case file:read(FD, 12) of
    {ok, <<T:64/big, Len:32/big>>} ->
      case file:read(FD, Len) of
        {ok, Bin} ->
          {Kind, PL} = binary_to_term(Bin),
          case Pred(T,Kind,PL,Acc,"") of
            {break, Res} ->
              Res;
            Res ->
              do_fold(Pred, Res, FD)
          end;
        eof ->
          Acc;
        {error, Reason} ->
          throw({error, Reason})
      end;
    eof -> Acc;
    {error, Reason} ->
      throw({error, Reason})
  end.

mfold(Pred, Init, Filenames) ->
  Options=[raw, read, binary, {read_ahead, 1024}],
  FDs=lists:map(
        fun(Filename) ->
            {ok, FD} = file:open(Filename, Options),
            {FD, Filename, undefined}
        end, Filenames),
  Res=do_mfold(Pred, Init, FDs),
  lists:foreach(fun(FD) -> file:close(FD) end, FDs),
  Res.

read_block({FD, Name, undefined}) ->
  case file:read(FD, 12) of
    {ok, <<T:64/big, Len:32/big>>} ->
      case file:read(FD, Len) of
        {ok, Bin} ->
          {Kind, PL} = binary_to_term(Bin),
          {FD, Name, {T, Kind, PL}};
        eof ->
          {FD, Name, eof};
        {error, Reason} ->
          throw({error, Name, Reason})
      end;
    eof ->
      {FD, Name, eof};
    {error, Reason} ->
      throw({error, Name, Reason})
  end;
read_block(Any) ->
  Any.

get_t({_, _, undefined}=A) -> get_t(read_block(A));
get_t({_, _, {T,_,_}}=A) -> {T,A};
get_t({T, {_, _, {T,_,_}}=A}) -> {T,A};
get_t({_, _, eof}=A) -> {eof,A};
get_t({eof, {_, _, eof}=A}) -> {eof,A}.

pick_payload({_,{FD,File,Payload}}) ->
  {Payload,{FD,File,undefined}}.

do_mfold(Pred, Acc, FDs) ->
  [F0|Rest]=lists:keysort(1,lists:map(fun get_t/1, FDs)),
  {P,{_,Filename,_}=F1}=pick_payload(F0),
  case P of
    eof ->
      Acc;
    {T,Kind,PL} ->
          case Pred(T,Kind,PL,Acc,Filename) of
            {break, Res} ->
              Res;
            Res ->
              do_mfold(Pred, Res, [F1|Rest])
          end
  end.





