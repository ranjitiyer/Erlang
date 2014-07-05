%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2014 10:43 AM
%%%-------------------------------------------------------------------
-module(semaphore).
-author("ranj4711").

%% API
-export([new/1, acquire/1, release/1, loop/2]).

new(Permits) ->
  Semaphore = spawn(?MODULE, loop, [Permits, Permits]),
  {ok, Semaphore}.

acquire(Semaphore) ->
  Semaphore ! {self(), acquire},
  receive
    granted ->
      granted
  end.

release(Semaphore) ->
  Semaphore ! {self(), release},
  receive
    released ->
      released;
    ignored ->
      ignored
  end.

loop(Permits, 0) ->
  io:format("~B ~n", [0]),
  receive
    {From, release} ->
      From ! released,
      loop(Permits, 1)
  end;

loop(Permits, Available) when Available>0 ->
  io:format("Available = ~B, Total = ~B ~n", [Available, Permits]),
  receive
    {From, acquire} ->
      From ! granted,
      loop(Permits, Available-1);
    {From, release} ->
      if
        Available < Permits ->
          From ! released,
          loop(Permits, Available+1);
        true ->
          From ! ignored,
          loop(Permits, Available)
      end
  end.

