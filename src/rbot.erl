-module(rbot).
-behaviour(application).

-export([start/2, stop/1, init/0, loop/2]).
-include("priv_keys.hrl").
-on_load(init/0).

init() ->
  application:ensure_all_started(crypto),
  application:ensure_all_started(public_key),
  application:ensure_all_started(ssl),
  application:ensure_all_started(inets),
  ok.

start(_Type, _Args) ->
  case get_token() of
    nil -> {error, "Could not refresh token"};
    Token -> {ok, spawn(?MODULE, loop, [Token, erlang:system_time(seconds)*0.0])}
  end.

stop(_State) ->
  ok.

loop(Token, Time) ->
  Messages = get_messages(Token),
  Unread = lists:takewhile(fun ({[_Kind, {<<"data">>, {Msg}}]}) -> proplists:get_value(<<"created_utc">>, Msg) >= Time end, Messages),
  N_Time = hd([process_message(Msg, Token) || {[_Kind, {<<"data">>, {Msg}}]} <- Unread]++[Time]),
  timer:sleep(10000),
  io:fwrite("Time is: ~f~n", [N_Time]),
  loop(get_token(), N_Time).

get_messages(Token) ->
  {ok,{{"HTTP/1.1",200,"OK"},
       _Content, Result}} = httpc:request(get,
                                          {"https://oauth.reddit.com/message/mentions.json",
                                           [{"Authorization", "bearer "++Token},
                                            {"User-Agent", "TJ ? Auto_TJ"}]}, [], []),
  {Json} = jiffy:decode(Result),
  {Data} = proplists:get_value(<<"data">>, Json),
  proplists:get_value(<<"children">>, Data).

process_message(Msg_Proplist, Token) ->
  timer:sleep(1000),
  M_Name = proplists:get_value(<<"name">>, Msg_Proplist),
  M_Body = proplists:get_value(<<"body">>, Msg_Proplist),
  Body = "api_type=json&return_rtjson=false&text=\"hello!\"&thing_id="++binary_to_list(M_Name),
  {ok,
   {{"HTTP/1.1",200,"OK"},
    _Content,
    Result}} = httpc:request(post,
                {"https://oauth.reddit.com/api/comment",
                 [{"Authorization", "bearer "++Token},
                  {"User-Agent", "TJ ? Auto_TJ"}],
                 "application/x-www-form-urlencoded",
                 Body
                 },
                [{ssl,[]}],
                []),
  io:fwrite("Result:~p~n", [Result]),
  proplists:get_value(<<"created_utc">>, Msg_Proplist).

get_token() ->
  Body = "grant_type=refresh_token&refresh_token="++?REFRESH_TOKEN,
  {ok,
   {{"HTTP/1.1",200,"OK"},
    _Content,
    Result}} = httpc:request(post,
                {"https://www.reddit.com/api/v1/access_token",
                 [{"Authorization", "Basic " ++ base64:encode_to_string(?REDDIT_PERSONAL ++ ":" ++ ?REDDIT_SECRET)}],
                 "application/x-www-form-urlencoded",
                 Body
                 },
                [{ssl,[]}],
                []),
  case string:prefix(Result, "{\"access_token\": \"") of
    nomatch -> nil;
    Str -> lists:takewhile(fun ($") -> false; (_) -> true end, Str)
  end.

