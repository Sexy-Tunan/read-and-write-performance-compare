%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 10月 2025 15:00
%%%-------------------------------------------------------------------
-module(tester).
-author("caigou").

-behavior(gen_server).
%% API
-export([start/0, stop/0]).
-export([test_process_read_write/0, test_ets_read_write/0, test_dets_read_write/0]).
-export([test_ram_copies_read_write/0, test_disc_copies_read_write/0, test_disc_only_copies_read_write/0]).
-export([test_ram_copies_read_write_with_dirty/0, test_disc_copies_read_write_with_dirty/0, test_disc_only_copies_read_write_with_dirty/0]).
%%-export([process_read/1, process_write/1,ets_read/2,ets_write/2, dets_read/2, dets_write/2, mnesia_read/2,mnesia_write/2]).
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
	gen_server:call(?MODULE, dets, 100000).

test_ram_copies_read_write() ->
	gen_server:call(?MODULE, ram_copies).

test_disc_copies_read_write() ->
	gen_server:call(?MODULE, disc_copies).

test_disc_only_copies_read_write() ->
	gen_server:call(?MODULE, disc_only_copies).

test_ram_copies_read_write_with_dirty() ->
	gen_server:call(?MODULE, {ram_copies,dirty_read_write}).

test_disc_copies_read_write_with_dirty() ->
	gen_server:call(?MODULE, {disc_copies,dirty_read_write}).

test_disc_only_copies_read_write_with_dirty() ->
	gen_server:call(?MODULE, {disc_only_copies,dirty_read_write},100000).


%% =============================
%% gen_server 回调模块
%% 初始化
init([]) ->
	process_flag(trap_exit, true),
	%% 初始化mnesia
	mnesia:create_schema([node()]),
	mnesia:start(),

	%% 构建一万条用于初始化的记录
	InitRecords = lists:map(
		fun(Item) -> #user{
			name = unicode:characters_to_binary(io_lib:format("Robot_~p",[Item]), utf8,utf8),
			password = unicode:characters_to_binary("123456789,,..", utf8,utf8),
			create_time = calendar:now_to_local_time(os:timestamp()),
			personal_signature = unicode:characters_to_binary(io_lib:format("我是Robot_~p",[Item]), utf8,utf8)
		} end,
		lists:seq(1,10000)
	),

	%% 构建一万条用于写读测试的记录
	TestRecords = lists:map(
		fun(Item) -> #user{
			name = unicode:characters_to_binary(io_lib:format("Robot_~p",[Item]), utf8,utf8),
			password = unicode:characters_to_binary("123456789,,..", utf8,utf8),
			create_time = calendar:now_to_local_time(os:timestamp()),
			personal_signature = unicode:characters_to_binary(io_lib:format("我是Robot_~p",[Item]), utf8,utf8)
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

%%	{TimeWrite, _} = timer:tc(?MODULE, process_write, [maps:get(test_records,State)], millisecond),
%%	{TimeRead, _} = timer:tc(?MODULE, process_read, [maps:get(test_records,State)], millisecond),ond),
	{TimeWrite, _} = timer:tc(fun() -> process_write(maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> process_read(maps:get(test_records,State)) end),
	erase(),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call(ets, _From, State) ->
	%% 将一万条初始化记录插入ets
	EtsSet = ets:new(user,[set,public,{keypos, #user.name}]),
	lists:foreach(
		fun(Record) -> ets:insert(EtsSet, Record) end,
		maps:get(init_records, State)
	),

	{TimeWrite, _} = timer:tc(fun() -> ets_write(EtsSet, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> ets_read(EtsSet, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call(dets, _From, State) ->
	%% 将一万条初始化记录插入dets
	{ok, DetsSet} = dets:open_file(user, [{file,"data/user.dets"},{keypos, 2},{repair,true},{type,set}]),
	lists:foreach(
		fun(Record) -> dets:insert(DetsSet, Record) end,
		maps:get(init_records, State)
	),

	{TimeWrite, _} = timer:tc(fun() -> dets_write(DetsSet, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> dets_read(DetsSet, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call(ram_copies, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为ram_copies
	case mnesia:create_table(user1, [{attributes, record_info(fields,user)},{ram_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user1,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_write(user1, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_read(user1, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call(disc_copies, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为disc_copies
	case mnesia:create_table(user2, [{attributes, record_info(fields,user)},{disc_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user2,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_write(user2, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_read(user2, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call(disc_only_copies, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为disc_only_copies
	case mnesia:create_table(user3, [{attributes, record_info(fields,user)},{disc_only_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user3,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_write(user3, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_read(user3, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call({ram_copies, dirty_read_write}, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为ram_copies
	case mnesia:create_table(user4, [{attributes, record_info(fields,user)},{ram_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user4,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_dirty_write(user4, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_dirty_read(user4, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call({disc_copies, dirty_read_write}, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为disc_copies
	case mnesia:create_table(user5, [{attributes, record_info(fields,user)},{disc_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user5,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_dirty_write(user5, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_dirty_read(user5, maps:get(test_records,State)) end),
	{reply, {ok, TimeWrite, TimeRead}, State};

handle_call({disc_only_copies, dirty_read_write}, _From, State) ->
	%% 将一万条初始化记录插入mnesia中，模式为disc_only_copies
	case mnesia:create_table(user6, [{attributes, record_info(fields,user)},{disc_only_copies, [node()]},{type,set},{record_name,user}]) of
		{atomic, ok} ->
			F = fun() -> lists:foreach(
				fun(Record) -> mnesia:write(user6,Record,write) end,
				maps:get(init_records, State)
			) end,
			mnesia:activity(transaction, F);
		{aborted, Reason} -> io:format("建表失败，原因：[~p]", [Reason])
	end,

	{TimeWrite, _} = timer:tc(fun() -> mnesia_dirty_write(user6, maps:get(test_records,State)) end),
	{TimeRead, _} = timer:tc(fun() -> mnesia_dirty_read(user6, maps:get(test_records,State)) end),
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
%%	mnesia:stop(),
	io:format("termanate停止应用~n"),
	ok.

process_write(Records) ->
	lists:foreach(
		fun(Record) -> put(Record#user.name, Record) end,
		Records
	),
	ok.
process_read(Records) ->
	lists:foreach(
		fun(Record) -> get(Record#user.name) end,
		Records
	),
	ok.

ets_write(EtsSet, Records) ->
	lists:foreach(
		fun(Record) -> ets:insert(EtsSet, Record) end,
		Records
	),
	ok.

ets_read(EtsSet, Records) ->
	lists:foreach(
		fun(Record) -> ets:lookup(EtsSet, Record#user.name) end,
		Records
	),
	ok.

dets_write(DetsSet, Records) ->
	lists:foreach(
		fun(Record) -> dets:insert(DetsSet, Record) end,
		Records
	),
	ok.

dets_read(DetsSet, Records) ->
	lists:foreach(
		fun(Record) -> dets:lookup(DetsSet, Record#user.name) end,
		Records
	),
	ok.

mnesia_write(TableName, Records) ->
	lists:foreach(
		fun(Record) -> mnesia:sync_transaction(fun() -> mnesia:write(TableName, Record, write) end) end,
		Records
	),
%%	F = fun() -> lists:foreach(
%%		fun(Record) -> mnesia:write(TableName, Record, write) end,
%%		Records
%%	) end,
%%	mnesia:sync_transaction(F),
	ok.

mnesia_read(TableName,Records) ->
	lists:foreach(
		fun(Record) -> mnesia:sync_transaction(fun() -> mnesia:read(TableName, Record#user.name) end) end,
		Records
	),
%%	F = fun() -> lists:foreach(
%%		fun(Record) -> mnesia:read(TableName, Record#user.name) end,
%%		Records
%%	) end,
%%	mnesia:sync_transaction(F),
	ok.


mnesia_dirty_write(TableName, Records) ->
	lists:foreach(
		fun(Record) -> mnesia:dirty_write(TableName, Record) end,
		Records
	),
	ok.

mnesia_dirty_read(TableName,Records) ->
	lists:foreach(
		fun(Record) -> mnesia:dirty_read(TableName, Record#user.name) end,
		Records
	),
	ok.