%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2014 10:39 AM
%%%-------------------------------------------------------------------
-module(http).
-author("ranj4711").

%% API
-export([post/3, get_status/1, get_reason/1, get_body/1, post_upload/4]).

get() ->
  ok.

post(URL,Headers,Params) ->
  ParamsList = lists:map(
    fun(Tuple) ->
      string:join([element(1, Tuple),element(2, Tuple)], "=")
    end,
    Params),
  RequestBody = string:join(ParamsList, "&"),

  httpc:request(post, {
    URL,
    Headers, %% headers
    "application/x-www-form-urlencoded", %% content-type
    RequestBody}, [], []).

post_attachment() ->
  ok.

get_status(Response) ->
  case Response of
    {ok, {{_,Status,_},_,_}} ->
      Status
  end.

get_reason(Response) ->
  case Response of
    {ok, {{_,_,Reason},_,_}} ->
      Reason
  end.

get_body(Response) ->
  case Response of
    {ok, {{_,_,_}, _, Body}} ->
      Body
  end.


post_upload(URL,Headers,Params,FileTuple) ->
  Boundary = "------------a450glvjfEoqerAc1p431paQlfDac152cadADfd",

  %% File data
  {FileParam,FileName} = FileTuple,
  Data = binary_to_list(element(2, file:read_file(FileName))),

  %% Build body with params and file contents
  Body = format_multipart_formdata(
    Boundary,
    Params,
    [{FileParam, filename:basename(FileName), Data}]
  ),

  %% Content type and union the supplied headers with ours
  ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
  Headers2 = Headers ++ [{"Content-Length", integer_to_list(length(Body))}],

  %% Make the request
  httpc:request(post,{URL,Headers2,ContentType,Body}, [], []).

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