-module(clog_sink_file).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-11-15").

-behaviour(gen_server).

-define(DEFAULT_SYNC_INTERVAL, 1000).
-define(DEFAULT_SYNC_SIZE, 1024*64). %% 64kb


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name,#{filename:=_}=Opts) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts#{name=>Name}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#{filename:=_}=Args) ->
  self() ! init,
  Defaults=#{sync_size=>?DEFAULT_SYNC_SIZE,
             sync_interval=>?DEFAULT_SYNC_INTERVAL,
             count=>5,
             size=>1 bsl 24
            },
  {ok, maps:merge( Defaults, Args#{unintialized=>true}) }.

handle_call(_Request, _From, State) ->
  lager:notice("Unknown call ~p",[_Request]),
  {reply, unhandled, State}.

handle_cast(_Msg, State) ->
  lager:notice("Unknown cast ~p",[_Msg]),
  {noreply, State}.

handle_info({heartbeat, PreT},
            #{name:=Name,
              htimer:=T,
              filename:=Filename,
              size:=MaxSize}=State) ->
  erlang:cancel_timer(T),
  State1=case is_rotation_needed(Filename, MaxSize) of
           true ->
             rotate(State);
           false ->
             State
         end,
  Timestamp=erlang:system_time(),
  T1=erlang:send_after(2000,self(),{heartbeat,Timestamp}),
  if(Timestamp-PreT < 3000) ->
      ets:insert(stout_alive,{Name,Timestamp});
    true -> busy
  end,
  {noreply, State1#{htimer=>T1}};

handle_info(init, #{filename:=F,
                    unintialized:=true,
                    sync_size:=SyncSize,
                    sync_interval:=SyncInterval,
                    name:=Name
                   }=State) ->
  {ok, {FD, Inode, _Size}} = open_logfile(F, {SyncSize, SyncInterval}),
  ets:insert(stout_alive,{Name,erlang:system_time()}),
  Timestamp=erlang:system_time(),
  T=erlang:send_after(2000,self(),{heartbeat,Timestamp}),
  {noreply, maps:remove(unintialized,
                        State#{
                          htimer=>T,
                          fd=>FD,
                          inode=>Inode
                         })};

handle_info(rotate, State) ->
  {noreply, rotate(State)};

handle_info({log, Timestamp, Kind, PropList}, #{fd:=FD}=State) when
    is_integer(Timestamp),
    is_atom(Kind),
    is_list(PropList)->
  Bin=erlang:term_to_binary({Kind,PropList}),
  _ = file:write(FD, [<<Timestamp:64/big,(size(Bin)):32/big>>, Bin]),
  {noreply, State};

handle_info(_Info, State) ->
  lager:notice("Unknown info  ~p",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

is_rotation_needed(Filename, Limit) ->
  {ok, FInfo} = file:read_file_info(Filename),
  (FInfo#file_info.size>Limit).

file_exists(Filename) ->
  case file:read_file_info(Filename) of
    {error,enoent} -> false;
    {ok, _} -> true
  end.

rotate(Filename, Ext, Max) when Ext==Max ->
  F=Filename++"."++integer_to_list(Ext),
  case file_exists(F) of
    true -> file:delete(F);
    false -> ok
  end;

rotate(Filename, Ext, Max) ->
  F1=case Ext of
       -1 -> Filename;
       _ -> Filename++"."++integer_to_list(Ext)
     end,
  case file_exists(F1) of
    true -> 
      F2=Filename++"."++integer_to_list(Ext+1),
      rotate(Filename, Ext+1, Max),
      file:rename(F1, F2);
    false -> 
      enoent
  end.

rotate(#{fd:=FD,
         filename:=F,
         sync_size:=SyncSize,
         sync_interval:=SyncInterval,
         count:=Max
        }=State) ->
  file:close(FD),
  rotate(F,-1,Max),
  {ok, {FD1, Inode1, _Size}} = open_logfile(F, {SyncSize, SyncInterval}),
  State#{
    fd=>FD1,
    inode=>Inode1
   }.

open_logfile(Name, Buffer) ->
    case filelib:ensure_dir(Name) of
        ok ->
            Options = [append, raw] ++
            case  Buffer of
                {Size, Interval} when is_integer(Interval), Interval >= 0, is_integer(Size), Size >= 0 ->
                    [{delayed_write, Size, Interval}];
                _ -> []
            end,
            io:format("Opening file opts ~p~n",[Options]),
            case file:open(Name, Options) of
                {ok, FD} ->
                    case file:read_file_info(Name) of
                        {ok, FInfo} ->
                            Inode = FInfo#file_info.inode,
                            {ok, {FD, Inode, FInfo#file_info.size}};
                        X -> X
                    end;
                Y -> Y
            end;
        Z -> Z
    end.


