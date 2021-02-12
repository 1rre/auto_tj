-module(rbot).
-behaviour(application).

-export([start/2, stop/1, init/0, loop/1]).
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
    Token -> {ok, spawn(?MODULE, loop, [Token])}
  end.

stop(_State) ->
  spawn(application, start, [rbot]),
  ok.

loop(Token) ->
  Messages = get_messages(Token),
  [process_message(Msg, Token) || {[_Kind, {<<"data">>, {Msg}}]} <- Messages],
  timer:sleep(10000),
  loop(get_token()).

get_messages(Token) ->
  {ok,{{"HTTP/1.1",200,"OK"},
       _Content, Result}} = httpc:request(get,
                                          {"https://oauth.reddit.com/message/unread.json",
                                           [{"Authorization", "bearer "++Token},
                                            {"User-Agent", "TJ ? Auto_TJ"}]}, [], []),
  {Json} = jiffy:decode(Result),
  {Data} = proplists:get_value(<<"data">>, Json),
  proplists:get_value(<<"children">>, Data).

process_message(Msg_Proplist, Token) ->
  timer:sleep(2000),
  M_Name = proplists:get_value(<<"name">>, Msg_Proplist),
  M_Body = proplists:get_value(<<"body">>, Msg_Proplist),
  case re:run(M_Body, "\\[\\d+d\\d+\\]", [{capture, all, list}, global]) of
    nomatch -> read_message(M_Name, Token);
    {match, Rolls} ->
      Result = respond_message(Rolls, M_Name, Token),
      case jiffy:decode(Result) of
        {<<"json">>, {<<"errors">>, [], _}} -> read_message(M_Name, Token);
        _ -> exit("error sending message")
      end
  end.

respond_message(Rolls, Msg, Token) ->
  F_Rolls = lists:join(", ", [Roll || [Roll] <- Rolls]),
  Body = "api_type=json&return_rtjson=false&text="++F_Rolls++"&thing_id="++binary_to_list(Msg),
  io:fwrite("~s~n", [Body]),
  {ok,
  {{"HTTP/1.1",200,"OK"},
    _Content,
    Result}} = httpc:request(post,
                {"https://oauth.reddit.com/api/comment",
                [{"Authorization", "bearer "++Token},
                  {"User-Agent", "TJ / Auto_TJ"}],
                "application/x-www-form-urlencoded",
                Body},
                [{ssl,[]}],
                []),
  io:fwrite("Result:~p~n", [Result]),
  Result.

read_message(Msg, Token) ->
  Read = "id="++binary_to_list(Msg),
  {ok,
   {{"HTTP/1.1",200,"OK"},
   _Read_Content,
   Read_Res}} = httpc:request(post,
                              {"https://oauth.reddit.com/api/read_message",
                               [{"Authorization", "bearer "++Token},
                                {"User-Agent", "TJ / Auto_TJ"}],
                               "application/x-www-form-urlencoded",
                               Read},
                              [{ssl,[]}],
                              []),
      io:fwrite("~s~n", [Read_Res]).
  
get_token() ->
  Body = "grant_type=refresh_token&refresh_token="++?REFRESH_TOKEN,
  {ok,
   {{"HTTP/1.1",200,"OK"},
    _Content,
    Result}} = httpc:request(post,
                {"https://www.reddit.com/api/v1/access_token",
                 [{"Authorization", "Basic " ++ base64:encode_to_string(?REDDIT_PERSONAL ++ ":" ++ ?REDDIT_SECRET)},
                  {"User-Agent", "TJ / Auto_TJ"}],
                 "application/x-www-form-urlencoded",
                 Body
                 },
                [{ssl,[]}],
                []),
  case string:prefix(Result, "{\"access_token\": \"") of
    nomatch -> nil;
    Str -> lists:takewhile(fun ($") -> false; (_) -> true end, Str)
  end.

