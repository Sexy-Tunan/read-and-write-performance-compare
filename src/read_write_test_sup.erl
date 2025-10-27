%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 10月 2025 16:59
%%%-------------------------------------------------------------------
-module(read_write_test_sup).
-author("Administrator").
-behavior(supervisor).
%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok,
		{
			%% 重启策略
			{one_for_one, 3, 10},
			%% Children
			[
				{
					tester,
					{tester, start, []},
					transient,
					5000,
					worker,
					[tester]
				}
			]
		}
	}.
