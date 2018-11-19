-module(stout_reader).
-export([fold/3]).

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
          case Pred(T,Kind,PL,Acc) of
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

          




