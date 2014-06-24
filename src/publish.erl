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
-import(util, [println/1, println/2, current_time/0, readlines/1]).

%% API
-export([publish/0, get_machines/4, get_token/4]).


publish() ->
  current_time(),

  % We need HTTP
  inets:start(),

  %% Get inputs
  println("This tool publishes services defined in pipe-delimited text file."),
  println("Enter: [server] [port] [user] [password] [Path to pipe-delimited text file]"),
  Line = string:strip(io:get_line(">"), right, $\n),
  {Machine, Port, Admin, Pwd, InputFile} = list_to_tuple(string:tokens(Line, " ")),

  %% URLs
  ContextUrl  = lists:concat(["http://", Machine, ":", Port, "/arcgis/admin"]),
  ServicesUrl = lists:concat(["http://", Machine, ":", Port, "/arcgis/rest/services"]),

  %% Parse the input file and get a list of Service Infos
  AllLines = readlines(InputFile),

  %% Turn lines into list of records
  ServiceInfos = make_serviceinfos(AllLines, []),

  %% Admin token
  Token = get_token(Admin, Pwd, Machine, Port).

-spec make_serviceinfos([string()], [serviceinfo()]) -> [serviceinfo()].
make_serviceinfos([Line | Lines], Infos) ->
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
      make_serviceinfos(Lines, [Infos |
        #serviceinfo{sd = SDPath, folder = FldrName, service = ServiceName, cluster = ClusterName}])
  end,
  Infos.

get_machines(Machine, Port, User, Password) ->
  inets:start(),
  Token = get_token(Machine, Port, User, Password),
  URL = lists:concat(["http://", Machine, ":", Port, "/arcgis/admin/machines"]),
  Params = [{"token", Token}, {"f","pjson"}],
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
    ReqBody }, [], []) of
    {ok, {{Version,Status, Reason}, Headers, ResBody}} ->
      JsonResponse = mochijson2:decode(ResBody),

      % get machines array
      MachinesArray = json:get_array(JsonResponse, <<"machines">>),

      % pull out admin urls for each
      MachineUrls = lists:map(
        fun(MachineJson) ->
          json:get_string(MachineJson, <<"adminURL">>)
        end,
        MachinesArray),

      % return the list
      MachineUrls
  end.


get_token(Machine, Port, User, Password) ->
  inets:start(),
  URL = lists:concat(["http://", Machine, ":", Port, "/arcgis/admin/generateToken"]),
  Params = [
    {"username", User},
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
                            ReqBody }, [], []) of
    {ok, {{Version,Status, Reason}, Headers, ResBody}} ->
      JsonResponse = mochijson2:decode(ResBody),
      Token = json:get_string(JsonResponse, <<"token">>),
      Token
  end.

