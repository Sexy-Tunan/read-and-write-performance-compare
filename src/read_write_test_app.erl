%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 10月 2025 17:14
%%%-------------------------------------------------------------------
-module(read_write_test_app).
-author("caigou").
-behavior(application).

%% API
-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
	read_write_test_sup:start_link().

stop(_State) ->
	ok.