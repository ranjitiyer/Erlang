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
-export([println/1, println/2, current_time/0, readLines/1]).

println(Format) ->
  println(Format, []).

println(Format, Args) ->
  io:format(Format, Args),
  io:format("~n").

readLines(File) ->
  case file:open(File, [read]) of
    {ok, Fd} ->
      readLine(Fd);
    {error, Reason} ->
      fail
  end.

readLine(Fd) ->
  case file:read_line(Fd) of
    {ok, Data} ->
      [Data | readLine(Fd)];
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
