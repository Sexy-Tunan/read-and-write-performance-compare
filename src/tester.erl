%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 10月 2025 16:20
%%%-------------------------------------------------------------------
-module(tester).
-author("Administrator").

-behavior(gen_server).
%% API
-export([start/0, stop/0]).
-export([test_process_read_write/0, test_ets_read_write/0, test_dets_read_write/0]).
-export([test_ram_copies_read_write/0, test_disc_copies_read_write/0, test_disc_only_copies_read_write/0]).
-export([]).
%% gen_server 回调接口
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).


-record(user,{name,password,create_time,personal_signature}).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

test_process_read_write() ->
	gen_server:call(?MODULE, process).

test_ets_read_write() ->
	gen_server:call(?MODULE, ets).

test_dets_read_write() ->
	gen_server:call(?MODULE, dets).

test_ram_copies_read_write() ->
	gen_server:call(?MODULE, ram_copies).

test_disc_copies_read_write() ->
	gen_server:call(?MODULE, disc_copies).

test_disc_only_copies_read_write() ->
	gen_server:call(?MODULE, disc_only_copies).


%% =============================
%% gen_server 回调模块
%% 初始化
init([]) ->
	%% 初始化mnesia
	mnesia:create_schema([node()]),
	mnesia:start(),

	%% 构建一万条用于初始化的记录
	InitRecords = lists:map(
		fun(item) -> #user{
			name = io_lib:format("Robot_~p",[item]),
			password = "123456789,,..",
			create_time = calendar:now_to_local_time(os:timestamp()),
			personal_signature = io_lib:format("我是Robot_~p",[item])
		} end,
		lists:seq(1,10000)
	),

	%% 构建一万条用于写读测试的记录
	TestRecords = lists:map(
		fun(item) -> #user{
			name = io_lib:format("Robot_~p",[item]),
			password = "123456789,,..",
			create_time = calendar:now_to_local_time(os:timestamp()),
			personal_signature = io_lib:format("我是Robot_~p",[item])
		} end,
		lists:seq(10001,20000)
	),
	{ok, #{init_records => InitRecords, test_records => TestRecords}}.


handle_call(process, _From, State) ->
	%% 将一万条初始化记录插入process进程字典
	lists:foreach(
		fun(Record) -> put(Record#user.name, Record) end,
		maps:get(init_records, State)
	),

	{TimeWrite, _} = timer:tc(?MODULE, process_write, [State]),
	{TimeRead, _} = timer:tc(?MODULE, process_read, [State]),
	{reply, {ok, TimeWrite, TimeRead}, State};



handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	io:format("收到停止信号，准备关闭...~n"),
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	mnesia:stop(),
	ok.

process_write(State) ->
	lists:foreach(
		fun(Record) -> put(Record#user.name, Record) end,
		State
	),
	ok.

process_read(State) ->
	lists:foreach(
		fun(Record) -> get(Record#user.name) end,
		State
	),
	ok.
