%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2014 12:59 PM
%%%-------------------------------------------------------------------
-module(publish).
-author("ranj4711").

-include("types.hrl").
-import(util, [println/1, println/2, current_time/0, readLines/1]).

%% API
-export([publish/0]).


publish() ->
  current_time(),

  %% Get inputs
  println("This tool publishes services defined in pipe-delimited text file."),
  println("Enter: [server] [port] [user] [password] [Path to pipe-delimited text file]"),
  io:get_line(">>"),
  Line = string:strip(io:get_line(">"), right, $\n),
  {Machine, Port, Admin, Pwd, InputFile} = list_to_tuple(string:tokens(Line, " ")),

  %% Context URLs
  ContextUrl  = lists:concat(["http://", Machine, ":", Port, "/arcgis/admin"]),
  ServicesUrl = lists:concat(["http://", Machine, ":", Port, "/arcgis/rest/services"]),

  %% Admin token
  Token = getToken(Admin, Pwd, Machine, Port),

  %% Parse the input file and get a list of Service Infos
  AllLines = readLines(InputFile),

  %% Turn lines into list of records
  ServiceInfos = makeServiceInfos(AllLines, []).

deleteUploads() ->
  io:format("Delete all uploaded items").


-spec makeServiceInfos(list()) -> [serviceinfo()].
makeServiceInfos([Line | Lines], Infos) ->
  case string:strip(Line) of
    %% ignore commented line
    [$# | _] ->
      ignore;
    %% ignore the blank line
    [$\n] ->
      ignore;
    _ ->
      [SD, Folder, Service, Cluster] = string:tokens(Line, "|"),
      [_, SDPath]      = string:tokens(SD, "="),
      [_, FldrName]    = string:tokens(Folder, "="),
      [_, ServiceName] = string:tokens(Service, "="),
      [_, ClusterName] = string:tokens(Cluster, "="),
      io:format("~s, ~s, ~s, ~s", [SDPath, FldrName, ServiceName, ClusterName]),
      makeServiceInfos(Lines, [Infos |
        #serviceinfo{sd = SDPath, folder = FldrName, service = ServiceName, cluster = ClusterName}])
  end,
  Infos.

getToken(Admin, Pwd, Machine, Port) ->
  io:format("Get the token").