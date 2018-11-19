-module(stout_pt).

-export([parse_transform/2]).


parse_transform(ASTForms, _Options) ->
  ModuleName=lists:foldl(
         fun(Form, Acc0) ->
             erl_syntax_lib:fold(
               fun({attribute, _, module, ModName},_) ->
                   ModName;
                  (_Any,Acc) ->
                   Acc
               end, Acc0, Form)
         end, undefined, ASTForms),

  F=fun({tree,application, Attr0, 
         {application,
          {remote,Line,{atom,_,stout},{atom,_,log}}=Remote,
          [Arg1,
           Arg2
          ]
         }
        }) ->
        Fixed={tree,application, Attr0, 
               {application,
                Remote,
                [Arg1,
                 Arg2,
                 {atom,Line,ModuleName},
                 {integer,Line,Line}
                 %, stacktrace(Line,Attr0)
                ]
               }
              },
        %io:format("++=> ~p~n~n",[Fixed]),
        erl_syntax:revert(Fixed);
       (Node) ->
        %io:format("===> ~p~n~n",[Node]),
        erl_syntax:revert(Node)
    end,
  [erl_syntax_lib:map(F, Form) || Form <- ASTForms].

%stacktrace(Line,Attr0) ->
%  {tree, application, Attr0,
%  {application,
%   {remote,Line,{atom,Line,erlang},{atom,Line,get_stacktrace}},
%   []}
%  }.
%%   [{call,Line,
%%     {'fun',Line,
%%      {clauses,
%%       [{clause,Line,[],[],
%%         [{'case',Line,
%%           {'catch',Line,{call,Line,{atom,Line,abs},[{atom,Line,x}]}},
%%           [{clause,Line,
%%             [{tuple,Line,
%%               [{atom,Line,'EXIT'},
%%                {tuple,Line,[{atom,Line,badarg},{var,Line,'Stacktrace'}]}]}],
%%             [],
%%             [{var,Line,'Stacktrace'}]}]}]}]}},
%%     []}].
%%
%%  erl_syntax:tree(
%%    [{call,1,
%%      {'fun',1,
%%       {clauses,
%%        [{clause,1,[],[],
%%          [{'case',1,
%%            {'catch',1,{call,1,{atom,1,abs},[{atom,1,x}]}},
%%            [{clause,1,
%%              [{tuple,1,
%%                [{atom,1,'EXIT'},
%%                 {tuple,1,
%%                  [{atom,1,badarg},
%%                   {var,1,'Stacktrace'}]}]}],
%%              [],
%%              [{var,1,'Stacktrace'}]}]}]}]}},
%%      []}]
%%   ).
%
%
