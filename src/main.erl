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
-export([scanf/0, print/1]).

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











