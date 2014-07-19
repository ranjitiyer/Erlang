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
-import(util, [println/1, println/2, current_time/0, readlines/1,get_object/2,get_string/2,get_array/2]).
-import(semaphore, [new/1,acquire/1,release/1]).
-import(http, [post/3, get_status/1, get_body/1, get_reason/1, post_upload/4]).
-import(ej, [get/2,get/3,set/3]).

%For testing
-export([make_serviceinfos/2]).

%% API
-export([publish/0, get_machines/4, get_token/4, upload/7, publish_sd/7]).

publish() ->
  current_time(),

  % We need HTTP
  inets:start(),

  %% Get inputs
  println("This tool publishes services defined in pipe-delimited text file."),
  println("Enter: [server] [port] [user] [password] [Path to pipe-delimited text file]"),
  Line = string:strip(io:get_line(">"), right, $\n),
  {Machine, Port, Admin, Pwd, InputFile} = list_to_tuple(string:tokens(Line, " ")),

  %% Parse the input file and get a list of Service Infos
  AllLines = readlines(InputFile),

  %% Turn lines into list of records
  ServiceInfos = make_serviceinfos(AllLines, []),

  % Limit to 10 uploaders
  {ok, UploadSemaphore} = semaphore:new(10),
  lists:foreach(
    fun(ServiceInfo) ->
      spawn(?MODULE, upload, [Machine, Port, Admin, Pwd, ServiceInfo, UploadSemaphore, self()])
    end, ServiceInfos),

  % Print the uploadids
  {ok, PublisherSemaphore} = semaphore:new(3),
  lists:foreach(
    fun (ServiceInfo) ->
      receive
        #serviceinfo{uploadid=UploadId,folder=Folder,service=Service,cluster=Cluster} ->
          spawn(?MODULE, publish_sd,
            [Machine, Port, Admin, Pwd,
              #serviceinfo{uploadid=UploadId,folder=Folder,service=Service,cluster=Cluster},
              PublisherSemaphore, self()])
      end
    end, ServiceInfos).

publish_sd(Machine,Port,User,Password,ServiceInfo,PublisherSemaphore,Caller) ->
  semaphore:acquire(PublisherSemaphore),

  Token = get_token(Machine,Port, User, Password),
  ServiceConfigUrl = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/uploads/" ++ ServiceInfo#serviceinfo.uploadid ++ "/serviceconfiguration.json",
  Response = http:post(ServiceConfigUrl,[],[{"token", Token}, {"f","pjson"}]),

  case http:get_status(Response) of
    200 ->
      ResponseJson = mochijson2:decode(http:get_body(Response)),

      % Override servicename, cluter and folder properties
      ej:set({"service","serviceName"}, ResponseJson, list_to_binary(ServiceInfo#serviceinfo.service)),
      ej:set({"service","clusterName"}, ResponseJson, list_to_binary(ServiceInfo#serviceinfo.cluster)),
      ej:set({"folderName"}, ResponseJson, list_to_binary(ServiceInfo#serviceinfo.folder)),

      % Submit publishing job
      Submitjoburl = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/rest/services/System/PublishingTools/GPServer/Publish%20Service%20Definition/submitJob",
      Params = [{"token", Token}, {"f","pjson"}, {"in_sdp_id", ServiceInfo#serviceinfo.uploadid},{"in_config_overwrite", mochijson2:encode(ResponseJson)}],
      %%io:format("URL is ~p~n", [Submitjoburl]),

      Response2 = http:post(Submitjoburl,[],Params),
      case http:get_status(Response2) of
        200 ->
          ResponseJson2 = mochijson2:decode(http:get_body(Response2)),
          io:format("Job ID ~p~n", [ej:get({"jobId"}, ResponseJson2)]),

          %% Check status
          StatusChecker = fun(JobId) ->
              %% Get status
              Status = "success",

              %% Check it
              case Status of
                "esriJobSucceeded" ->
                  success;
                "esriJobSubmitted" orelse "esriJobExecuting" ->
                  %% If now is less than expired
                  %% call status checker else timeout
                  timeout;
                "esriJobExecutionFailed" ->
                  error
              end
            end,

          %% Call it and wait for results
          case StatusChecker(ej:get({"jobId"}, ResponseJson2), self()) of
            timeout ->
              ok;
            error ->
              ok;
            success ->
              ok
          end;
      _ ->
          io:format("Job submission failed ~p~n", [http:get_reason(Response2)])
      end;
   _ ->
      io:format("Service config request failed ~p~n", [http:get_reason(Response)])
  end,

  semaphore:release(PublisherSemaphore).

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

  Response = http:post(URL,[],Params),
  case http:get_status(Response) of
    200 ->
      Body = http:get_body(Response),
      JsonResponse = mochijson2:decode(Body),
      MachinesList = json:get_array(JsonResponse, <<"machines">>),
      MachineUrls = lists:map(
        fun(MachineJson) ->
          json:get_string(MachineJson, <<"adminURL">>)
        end,
        MachinesList),

      %% Return the list of machines
      MachineUrls;
    _ ->
      io:format("Error ~p~n", http:get_reason(Response))
  end.

get_token(Machine, Port, User, Password) ->
  inets:start(),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/generateToken",
  Params = [
    {"username", User},
    {"password", Password},
    {"client", "requestip"},
    {"f","pjson"}
  ],

  Response = http:post(URL,[],Params),
  case http:get_status(Response) of
    200 ->
      Body = http:get_body(Response),
      JsonResponse = mochijson2:decode(Body),
      Token = json:get_string(JsonResponse, <<"token">>),
      Token;
    _ ->
      io:format("Error ~p~n", [http:get_reason(Response)])
  end.

upload(Machine, Port, User, Password,#serviceinfo{sd = SD} = ServiceInfo,Semaphore,Caller) ->
  semaphore:acquire(Semaphore),
  inets:start(),
  Token = get_token(Machine,Port, User, Password),
  URL = "http://" ++ Machine ++ ":" ++ Port ++ "/arcgis/admin/uploads/upload",

  Params = [{token, Token}, {f, "pjson"}],
  FileParam = {itemFile, SD},

  Response = http:post_upload(URL,[],Params,FileParam),

  case http:get_status(Response) of
    200 ->
      JsonResponse = mochijson2:decode(http:get_body(Response)),
      ItemObject = json:get_object(JsonResponse, <<"item">>),
      UploadId = json:get_string(ItemObject, <<"itemID">>),

      % Send reponse back to parent
      Caller ! ServiceInfo#serviceinfo{uploadid = UploadId};
    _ ->
      io:format("Error ~p~n",[http:get_reason(Response)])
  end,
  semaphore:release(Semaphore).
