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

%% API
-export([scanf/0]).

scanf() ->
  io:format("Hello World ~n"),
  Data = string:strip(io:get_line(">"), right, $\n),
  io:format("~s~n", [Data]),
  Tokens = string:tokens(Data, " "),
  {Machine, Port, Admin, Pwd, InputFile} = list_to_tuple(Tokens),
  io:format([Machine, Port, Admin, Pwd, InputFile]).




