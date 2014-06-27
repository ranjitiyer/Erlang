%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2014 1:15 PM
%%%-------------------------------------------------------------------
-module(main).
-author("ranj4711").

-import(file, []).

%% API
-export([scanf/0, print/1, getToken/4, pingtest/0, pongtest/1]).

scanf() ->
  io:format("Hello World ~n"),
  Data = string:strip(io:get_line(">"), right, $\n),
  io:format("~s~n", [Data]),
  Tokens = string:tokens(Data, " "),
  {Machine, Port, Admin, Pwd, InputFile} = list_to_tuple(Tokens),
  io:format([Machine, Port, Admin, Pwd, InputFile]).

print(File) ->
  case file:open(File, [read]) of
    {ok, Fd} ->
      AllLines = readAllLines(Fd),
      lists:foreach(fun (Line) ->
        case string:strip(Line) of
          %% ignore lines that are commented
          [$# | _] ->
            ignore;
          %% ignore blank lines
          [$\n] ->
            ignore;
          _ ->
            [SD, Folder, Service, Cluster] = string:tokens(Line, "|"),
            [_, SDPath]      = string:tokens(SD, "="),
            [_, FldrName]    = string:tokens(Folder, "="),
            [_, ServiceName] = string:tokens(Service, "="),
            [_, ClusterName] = string:tokens(Cluster, "="),
            io:format("~s, ~s, ~s, ~s", [SDPath, FldrName, ServiceName, ClusterName])
        end
        end, AllLines);
    {error, Reason} ->
      fail
  end.

readAllLines(Fd) ->
  case file:read_line(Fd) of
    {ok, Data} ->
      [Data | readAllLines(Fd)];
    eof ->
      []
  end.

getToken(Machine, Port, User, Password) ->
  inets:start(),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/generateToken",
  Params = [{"username", User},
            {"password", Password},
            {"client", "requestip"},
            {"f","pjson"}],

  ParamsList = lists:map(
      fun(Tuple) ->
        string:join([element(1, Tuple),element(2, Tuple)], "=")
      end,
    Params),

  ReqBody = string:join(ParamsList, "&"),
  io:format("~s~n",[ReqBody]),

  case httpc:request(post, {
        URL,
        [], %% headers
        "application/x-www-form-urlencoded", %% content-type
        ReqBody
      }, [], []) of
    {ok, {{Version,Status, Reason}, Headers, ResBody}} ->
      {struct,[
        {<<"token">>, Token}, {<<"expires">>, Expiration}
      ]} = mochijson2:decode(ResBody),
      Token
  end.

pingtest() ->
  Pid = spawn(main, pongtest, [self()]),
  Pid ! {"Hello"},
  receive
    {ok, Greeting} -> io:format("Ping received a response from pong ~n", [])
  end.

pongtest(Ref) ->
  receive
    {Greeting} ->
      io:format("Pong received ~s~n", [Greeting]),
      Ref ! {ok, "Greetings received"}
  end.











