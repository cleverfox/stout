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
                 {integer,Line,Line},
                 {tree, application, Attr0,
                 {application,
                  {remote,Line,{atom,Line,erlang},{atom,Line,get_stacktrace}},
                  []}
                 }
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

