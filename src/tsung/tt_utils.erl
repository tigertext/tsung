%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module contains help function for running tigertext testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(tt_utils).
-author('gye@tigertext.com').

-include("ts_macros.hrl").
-export([login_url/1, get_watermark/1, save_roster/1, message_url/1, group_message_url/1]).
-export([search_account/1, search_group/1, search_dist_list/1, store_for_validation/2]).
-export([start_post_test_auditing/0]).
start_post_test_auditing() ->
    try
      ts_msg_recorder_sup:flush_children(),
      {ok, {{_, 200, _}, _, _}} = httpc:request("http://localhost:8800/start_auditing?filename=%2Ftmp%2Fmsg_sent.txt")
    catch
        _:_ -> ignore
    end.
       
    
store_for_validation(message_id, Msg_Id) -> 
    ?LOGF("got message id=~p~n", [Msg_Id], ?DEB),
    ts_msg_recorder:record(Msg_Id);
store_for_validation(_, _) -> ignore.

%% in milliseconds
ts() ->
    {MSec, Seconds, MuSec} = now(),
    (Seconds+1000000*MSec)*1000 + MuSec div 1000.
   
save_roster({_Pid, DynVars}) ->
    try
        ?LOGF("Saving roster ~p ~n", [DynVars], ?DEB),
        {ok, Roster_Response} = ts_dynvars:lookup(roster_response, DynVars),
        Roster = parse_roster_response(Roster_Response, []),
        ?LOGF("Got roster: ~p~n", [Roster], ?DEB),
        Roster
    catch
        _:Reason ->
          ?LOGF("Got exception while handling poll response: ~p, stack: ~p~n", [Reason, erlang:get_stacktrace()], ?ERR),
          []
    end.
            
message_url({_Pid, DynVars}) ->
    {ok, Ttl} = ts_dynvars:lookup(ttl, DynVars),
    {ok, Cn} = ts_dynvars:lookup(cn_server, DynVars),
    {ok, Organization} = ts_dynvars:lookup(messaging_org, DynVars),
    {ok, Sender} = ts_dynvars:lookup(my_token, DynVars),
    {ok, Session} = ts_dynvars:lookup(session_id, DynVars),
    {ok, Cn} = ts_dynvars:lookup(cn_server, DynVars),
    Receiver = get_random_receiver(Sender),
    Time = now_for_timestamp(),
    Client_Id = uuid:to_string(uuid:srandom()),
    Url = ["{", "\"body\": \"test\", ", "\"sender\":", "\"", Sender, "\",", "\"recipient\":", "\"", Receiver, "\",",
           "\"sender_organization\": \"", Organization, "\",", "\"recipient_organization\": \"", Organization, "\",",
           "\"client_id\":\"", Client_Id, "\",", "\"dor\": \"0\",", 
           "\"ttl\":", "\"", Ttl, "\",", "\"xmlns\": \"tigertext:iq:message\",",  
           "\"status\": \"New\",", "\"ts\":", integer_to_binary(ts()), "}"],
    ?LOGF("url = ~p, session=~p~n", [Url, Session], ?DEB),
    Bin_Url = erlang:iolist_to_binary(Url),
    Final_Url = "/cn" ++ binary_to_list(Cn) ++ "/message?req=" ++ edoc_lib:escape_uri(binary_to_list(Bin_Url)) ++ "&session_id=" ++ binary_to_list(Session),
    ?LOGF("final url = ~p~n", [Final_Url], ?DEB),
    Final_Url.

group_message_url({_Pid, DynVars}) ->
    {ok, Ttl} = ts_dynvars:lookup(ttl, DynVars),
    {ok, Cn} = ts_dynvars:lookup(cn_server, DynVars),
    {ok, Organization} = ts_dynvars:lookup(messaging_org, DynVars),
    {ok, Sender} = ts_dynvars:lookup(my_token, DynVars),
    {ok, Session} = ts_dynvars:lookup(session_id, DynVars),
    {ok, Cn} = ts_dynvars:lookup(cn_server, DynVars),
    {ok, Group_Token} = ts_dynvars:lookup(group_token, DynVars),
    Client_Id = uuid:to_string(uuid:srandom()),
    Url = ["{", "\"body\": \"test\", ", "\"sender\":", "\"", Sender, "\",", "\"recipient\":", "\"", Group_Token, "\",",
        "\"sender_organization\": \"", Organization, "\",", "\"recipient_organization\": \"", Organization, "\",",
        "\"client_id\":\"", Client_Id, "\",", "\"dor\": \"0\",",
        "\"ttl\":", "\"", Ttl, "\",", "\"xmlns\": \"tigertext:iq:group:message\",",
        "\"status\": \"New\",", "\"ts\":", integer_to_binary(ts()), "}"],
    ?LOGF("url = ~p, session=~p~n", [Url, Session], ?DEB),
    Bin_Url = erlang:iolist_to_binary(Url),
    Final_Url = "/cn" ++ binary_to_list(Cn) ++ "/group_message?req=" ++ edoc_lib:escape_uri(binary_to_list(Bin_Url)) ++ "&session_id=" ++ binary_to_list(Session),
    ?LOGF("final url = ~p~n", [Final_Url], ?DEB),
    Final_Url.

get_random_receiver(Sender) ->
    {ok, R} = ts_file_server:get_random_line(message_receivers),
    ?LOGF("sender is ~p, receiver: ~p~n", [Sender, R], ?DEB),
    Trimed_R = trim(R),
    case Trimed_R of 
        Sender -> get_random_receiver(Sender);
        _ -> Trimed_R 
    end.

login_url({_Pid, DynVars}) ->
    {ok, Username} = ts_dynvars:lookup(username, DynVars),
    {ok, Password} = ts_dynvars:lookup(password, DynVars),
    Updated_Password = trim(Password),
    Url = ["{", "\"id\":", "\"", Username, "\", \"password\": \"", Updated_Password, "\", ", "\"ts\":", 
          integer_to_binary(ts()), "}"],
    Bin_Url = erlang:iolist_to_binary(Url),
    "/cn/connect?req=" ++ edoc_lib:escape_uri(binary_to_list(Bin_Url)).

trim(Token) -> binary:replace(Token, [<<"\r">>, <<"\n">>], <<>>, [global]). 

get_watermark({_Pid, DynVars}) ->
    Current_Watermark = init_watermark(ts_dynvars:lookup(watermark, DynVars)),
    try
        {ok, Poll_Response} = ts_dynvars:lookup(poll_response, DynVars),
        ?LOGF("Got poll response: ~p~n", [Poll_Response], ?DEB),
        WM = get_highest_watermark(Current_Watermark, Poll_Response),
        ?LOGF("Got new water mark: ~p~n", [WM], ?DEB),
        WM
    catch
       _:Reason ->
          ?LOGF("Got exception while handling poll response: ~p, stack: ~p~n", [Reason, erlang:get_stacktrace()], ?ERR),
          Current_Watermark
    end.

init_watermark(false) -> "0";
init_watermark({ok, V}) -> V.

get_highest_watermark(Current_Value, []) -> Current_Value;
get_highest_watermark(Current_Value, [{_, Props}|Others]) ->
    WM = proplists:get_value(<<"watermark">>, Props, <<"0">>),
    WM_List = binary_to_list(WM),
    case Current_Value < WM_List of
        true -> get_highest_watermark(WM_List, Others);
        false -> get_highest_watermark(Current_Value, Others)
    end.
parse_roster_response([], Result) -> Result;
parse_roster_response([First|T], Result) ->
    {_, Props} = First,
    Token = proplists:get_value(<<"token">>, Props),
    Org = proplists:get_value(<<"organization_id">>, Props),
    Namespace = proplists:get_value(<<"xmlns">>, Props),
    Current =  proplists:get_value(Namespace, Result, []),
    New_List = update_roster_by_type(Namespace, {Token, Org}, Current),
    parse_roster_response(T, lists:keystore(get_type(Namespace), 1, Result, New_List)).

-define(RETURN_FIELDS, [<<"token">>,<<"status">>,<<"display_name">>,
                        <<"first_name">>,<<"last_name">>,<<"username">>,
                        <<"phones">>,<<"emails">>,<<"api_keys">>,<<"pagers">>,
                        <<"department">>,<<"title">>,<<"organization_key">>,
                        <<"organization_name">>,<<"avatar">>,<<"account_type">>,
                        <<"activation_time">>,<<"welcome_email_sent">>,<<"name">>,
                        <<"replay_history">>,<<"is_public">>,<<"members">>]).
search_account({_pid, DynVars}) ->
    search_type(<<"account">>, DynVars).
search_group({_pid, DynVars}) ->
    search_type(<<"group">>, DynVars).
search_dist_list({_pid, DynVars}) ->
    search_type(<<"distribution_list">>, DynVars).
search_type(Type, DynVars) ->
    {ok, Organization} = ts_dynvars:lookup(messaging_org, DynVars),
    {ok, Term} = ts_dynvars:lookup(search_term, DynVars),
    EJson = {struct, [{<<"directory">>, [to_binary(Organization)]}, {<<"type">>, [Type]},
                      {<<"return_fields">>, ?RETURN_FIELDS}, {<<"bool">>, {struct,
                      [{<<"must">>, {struct, [{<<"any">>, to_binary(Term)}]}}]}}]},
    binary_to_list(erlang:iolist_to_binary(mochijson2:encode(EJson))).
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V.
get_type(<<"tigertext:entity:account">>) -> account;
get_type(<<"tigertext:entity:group">>) -> group;
get_type(<<"tigertext:entity:list">>) -> dist_list;
get_type(_) -> unknown.
update_roster_by_type(Namespace, {Token, Org}, List) ->
    update_roster_by_type_internal(get_type(Namespace), {Token, Org}, List).
update_roster_by_type_internal(Type, {Token, Org}, []) -> {Type, [{Token, Org}]};
update_roster_by_type_internal(Type, {Token, Org}, Current) -> {Type, [{Token, Org}|Current]}.

-spec now_for_timestamp() -> string().
now_for_timestamp() ->
    T   = {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    S   = calendar:datetime_to_gregorian_seconds(T),
    TS  = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~.10.0BZ",
                       [Year, Month, Day, Hour, Minute, Second, 0])).
