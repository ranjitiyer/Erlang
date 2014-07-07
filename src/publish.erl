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
-import(semaphore, [new/1,acquire/1,release/1]).
-import(json, [get_object/2,get_string/2,get_array/2]).

%For testing
-export([make_serviceinfos/2]).

%% API
-export([publish/0, get_machines/4, get_token/4, upload/7]).

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

  % Limit to 10 uploaders
  UploadSemaphore = semaphore:new(10),
  lists:foreach(
    fun(ServiceInfo) ->
      spawn(?MODULE, upload, [Machine, Port, Admin, Pwd, ServiceInfo, UploadSemaphore, self()])
    end, ServiceInfos),

  receive
    #serviceinfo{uploadid=UploadId,folder=Folder,service=Service,cluster=Cluster} ->
      % Spawn publishing tool and publish
      ok
  end,

  %% Admin token
  Token = get_token(Admin, Pwd, Machine, Port).

-spec make_serviceinfos([string()], [serviceinfo()]) -> [serviceinfo()].
make_serviceinfos([Line | Lines], Infos) ->
  case Line of
    %% commented line
    [$#|_] ->
      make_serviceinfos(Lines, Infos);
    %% blank line
    [] ->
      make_serviceinfos(Lines, Infos);
    _ ->
      [SD, Folder, Service, Cluster] = string:tokens(Line, "|"),

      [_, SDPath]      = string:tokens(SD, "="),
      [_, FldrName]    = string:tokens(Folder, "="),
      [_, ServiceName] = string:tokens(Service, "="),
      [_, ClusterName] = string:tokens(Cluster, "="),

      make_serviceinfos(Lines,  [#serviceinfo{
        sd = SDPath, folder = FldrName, service = ServiceName, cluster = ClusterName
      } | Infos])
  end;

make_serviceinfos([], Infos) ->
  Infos.

get_machines(Machine, Port, User, Password) ->
  inets:start(),
  Token = get_token(Machine, Port, User, Password),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/machines",
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

%% publish(Machine, Port, Admin, User, Pwd, ServiceInfo) ->
%%   ok.

get_token(Machine, Port, User, Password) ->
  inets:start(),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/generateToken",
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

upload(Machine, Port, User, Password,
    #serviceinfo{sd = SD} = ServiceInfo, Semaphore, Caller) ->
  semaphore:acquire(Semaphore),
  inets:start(),
  Token = get_token(Machine, Port, User, Password),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/uploads/upload",

  io:format("Preparing to upload ~p~n", [SD]),
  Data = binary_to_list(element(2, file:read_file(SD))),
  Boundary = "------------a450glvjfEoqerAc1p431paQlfDac152cadADfd",
  Body = format_multipart_formdata(Boundary, [{token, Token}, {f, "pjson"}],
    [{itemFile, filename:basename(SD), Data}]),
  ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
  Headers = [{"Content-Length", integer_to_list(length(Body))}],

  case httpc:request(post,{ URL,Headers,ContentType,Body}, [], []) of
    {ok, {{Version,Status, Reason}, ResponseHeaders, ResBody}} ->
      JsonResponse = mochijson2:decode(ResBody),

      % send response to caller
      ItemObject = json:get_object(JsonResponse, <<"item">>),
      UploadId = json:get_string(ItemObject, <<"itemID">>),
      Caller ! ServiceInfo#serviceinfo{uploadid = UploadId}
  end,
  semaphore:release(Semaphore).

%% @doc encode fields and file for HTTP post multipart/form-data.
%% @reference Inspired by <a href="http://code.activestate.com/recipes/146306/">Python implementation</a>.
format_multipart_formdata(Boundary, Fields, Files) ->
  FieldParts = lists:map(fun({FieldName, FieldContent}) ->
    [lists:concat(["--", Boundary]),
      lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\""]),
      "",
      FieldContent]
  end, Fields),
  FieldParts2 = lists:append(FieldParts),
  FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
     [lists:concat(["--", Boundary]),
      lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\"; filename=\"",FileName,"\""]),
      lists:concat(["Content-Type: ", "application/octet-stream"]),
      "",
      FileContent]
  end, Files),
  FileParts2 = lists:append(FileParts),
  EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
  Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
  string:join(Parts, "\r\n").

