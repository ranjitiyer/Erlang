%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2014 4:12 PM
%%%-------------------------------------------------------------------
-module(test).
-author("ranj4711").

-include("../src/types.hrl").
-import(publish, [upload/7, make_serviceinfos/2]).
-import(semaphore, [new/1,acquire/1,release/1]).
-import(util, [readlines/1]).

%% API
-export([
  test_upload/0,
  test_readalllines/0,
  test_makeserviceinfos/0,
  test_alluploads/0
]).

test_upload() ->
  {ok, Semaphore} = semaphore:new(1),
  publish:upload("localhost", "6080", "admin","admin",
    #serviceinfo{sd="c:/temp/Mobile_bv.sd"}, Semaphore, self()),
  receive
    #serviceinfo{uploadid = Id} ->
      io:format("Upload ID is ~p~n", [Id])
  end.

test_readalllines() ->
  File = "c:/work/erlang/publish/services.txt",
  Lines = util:readlines(File),
  io:format("~p~n", [Lines]).

test_makeserviceinfos() ->
  File = "c:/work/erlang/publish/services.txt",
  Lines = readlines(File),
  ServiceInfos = publish:make_serviceinfos(Lines, []),
  lists:foreach(fun(Info) ->
    io:format("SD is ~p, cluster is ~p ~n", [Info#serviceinfo.sd
      ,Info#serviceinfo.cluster])
    end, ServiceInfos).

test_alluploads() ->
  File = "c:/work/erlang/publish/services.txt",
  Lines = readlines(File),
  ServiceInfos = publish:make_serviceinfos(Lines, []),
  {ok, UploadSemaphore} = semaphore:new(10),
  lists:foreach(
    fun(ServiceInfo) ->
      spawn(publish, upload, ["localhost", "6080", "admin", "admin",
        ServiceInfo, UploadSemaphore, self()])
    end, ServiceInfos),

  receive
    #serviceinfo{uploadid=UploadId,folder=Folder,service=Service,cluster=Cluster} ->
      io:format("~p~n", [UploadId])
  end.




