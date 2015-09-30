-module(ts_msg_recorder).
-author('gye@tigertext.com').

-behaviour(gen_server).
-include("ts_macros.hrl").
%% api
-export([record/1, record/2, start_link/1, flush/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%%===================================================================
%%% init, terminate, code_change, info callbacks
%%%===================================================================

-record(state, {id :: string(), file :: pid(), msgs=[] :: [binary()]}).

record(Msg) -> record(get_server(), Msg).
record(Id, Msg) -> gen_server:cast(Id, {msg, Msg}).
flush(Id) -> gen_server:cast(Id, flush).
start_link(Id) -> gen_server:start_link({local, Id}, ts_msg_recorder, Id, []).

get_server() -> ts_msg_recorder_sup:get_random_child().
%% @private
-spec init(string()) -> {ok, #state{}}.
init(Id) ->
    ?LOGF("Starting ts_msg_recorder ~p~n", [Id], ?INFO),
    {ok, File} = file:open("/tmp/msg_sent.txt", [append]),
    process_flag(trap_exit, true),
    {ok, #state{id=Id, file=File}}.
%% @private
-spec terminate(atom(), #state{}) -> ok.
terminate(_Reason, #state{id=Id, msgs=Msgs, file=File}) ->
    ?LOGF("End of ts_msg_recorder ~p~n", [Id], ?INFO),
    flush_msgs(Msgs, File),
    file:sync(File),
    ok.
%% @private
-spec code_change(string(), #state{}, any()) -> {ok, {}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%% @private
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) -> {noreply, State}.
-define(MAX_LENGTH, 100).
handle_cast({msg, Msg_Id}, #state{msgs=Msgs, id=Id} = State) ->
   ?LOGF("got msg=~p at ~p~n", [Msg_Id, Id], ?DEB),
   Updated = [Msg_Id|Msgs],
   case length(Updated) > ?MAX_LENGTH of
       true -> flush_msgs(Updated, State#state.file),
               {noreply, State#state{msgs=[]}};
       false -> {noreply, State#state{msgs=Updated}}
   end; 
handle_cast(flush, #state{msgs=Msgs, id=Id} = State) ->
   ?LOGF("got flush at ~p~n", [Id], ?DEB),
   flush_msgs(Msgs, State#state.file),
   file:sync(State#state.file),
   {noreply, State#state{msgs=[]}}.

handle_call(Call, _From, State) ->
  lager:error("Invalid call:~p", [Call]),
  {reply, {error, invalid_request}, State}.

flush_msgs(Msgs, File) ->
    Out = [<<Msg/binary, "\r\n">> || Msg <- Msgs],
    ok = file:write(File, [<<Msg/binary, "\r\n">> || Msg <- Msgs]). 
