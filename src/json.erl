%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2014 3:27 PM
%%%-------------------------------------------------------------------
-module(json).
-author("ranj4711").

%% API
-export([get_string/2, get_array/2]).

get_string(MochiJson, Key) ->
  case get(MochiJson, Key) of
    Value when is_bitstring(Value) ->
      binary_to_list(Value);
    _ ->
      nil
  end.

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

get_array(MochiJson, Key) ->
  get(MochiJson, Key).