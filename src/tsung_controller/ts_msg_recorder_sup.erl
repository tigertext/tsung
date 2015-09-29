-module(ts_msg_recorder_sup).
-author('gye@tigertext.com').

-behaviour(supervisor).

-export([start_link/0, init/1, get_random_child/0, child_name/1, flush_children/0]).
-define(MAX_WORKER, 20).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

flush_children() ->
   [
      begin
        Name = whereis(ts_msg_recorder_sup:child_name(I)),
        ts_msg_recorder:flush(Name),
        timer:sleep(100)
      end ||
      I <- lists:seq(1, ?MAX_WORKER)
  ].
%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%% @hidden
-spec init([]) -> {ok, {{simple_one_for_one, 5, 60}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 5000, 60}, child(?MAX_WORKER)}}.

get_random_child() ->
    Id = random:uniform(?MAX_WORKER),
    list_to_atom("ts_msg_recorder_" ++ integer_to_list(Id)).

child_name(Id) -> list_to_atom("ts_msg_recorder_" ++ integer_to_list(Id)).
child(Num) ->
    [{child_name(I),
     {ts_msg_recorder, start_link, [child_name(I)]},
     permanent, 5000, worker, [ts_msg_recorder]}
     || I <- lists:seq(1, Num)].
