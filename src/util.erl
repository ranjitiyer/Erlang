%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2014 3:01 PM
%%%-------------------------------------------------------------------
-module(util).
-author("ranj4711").

%% API
-export([println/1, println/2, current_time/0, readlines/1,get_string/2, get_array/2, get_object/2]).

println(Format) ->
  println(Format, []).

println(Format, Args) ->
  io:format(Format, Args),
  io:format("~n").

readlines(File) ->
  case file:open(File, [read]) of
    {ok, Fd} ->
      readline(Fd);
    {error, Reason} ->
      fail
  end.

readline(Fd) ->
  case file:read_line(Fd) of
    {ok, Data} ->
      [string:strip(Data, both, $\n) | readline(Fd)];
    eof ->
      []
  end.

current_time() ->
  TS = {_,_,Micro} = os:timestamp(),

  {{Year,Month,Day},{Hour,Minute,Second}} =
    calendar:now_to_universal_time(TS),

  Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
    "Aug","Sep","Oct","Nov","Dec"}),

  io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w",
    [Day,Mstr,Year,Hour,Minute,Second,Micro]).



get_string(MochiJson, Key) ->
  case get(MochiJson, Key) of
    Value when is_bitstring(Value) ->
      binary_to_list(Value);
    _ ->
      nil
  end.

get_object(MochiJson, Key) ->
  get(MochiJson, Key).

get_array(MochiJson, Key) ->
  get(MochiJson, Key).

get(MochiJson, Key) ->
  case MochiJson of
    {struct, TupleArray} ->
      case lists:keyfind(Key,1,TupleArray) of
        false ->
          nil;
        {Key, Value} ->
          Value
      end;
    _ ->
      nil
  end.



