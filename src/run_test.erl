%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 10月 2025 15:00
%%%-------------------------------------------------------------------
-module(run_test).
-author("Administrator").

%% API
-export([run/0]).


run() ->
	%% 测试
	io:format("===========读写性能测试对比===============~n"),
	io:format("===========Process读写一万条数据性能===============~n"),
	{ok, ProcessWriteTime, ProcessReadTime} = tester:test_process_read_write(),
	io:format("Process在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [ProcessWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [ProcessReadTime]),

	io:format("===========ets读写一万条数据性能===============~n"),
	{ok, EtsWriteTime, EtsReadTime} = tester:test_ets_read_write(),
	io:format("ets在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [EtsWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [EtsReadTime]),

	io:format("===========dets读写一万条数据性能===============~n"),
	{ok, DetsWriteTime, DetsReadTime} = tester:test_dets_read_write(),
	io:format("dets在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [DetsWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [DetsReadTime]),

	io:format("===========ram_copies读写一万条数据性能===============~n"),
	{ok, RamWriteTime, RamReadTime} = tester:test_ram_copies_read_write(),
	io:format("ram_copies在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [RamWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [RamReadTime]),

	io:format("===========disc_copies读写一万条数据性能===============~n"),
	{ok, DiscCopiesWriteTime, DiscCopiesReadTime} = tester:test_disc_copies_read_write(),
	io:format("disc_copies在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [DiscCopiesWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [DiscCopiesReadTime]),

	io:format("===========disc_only_copies读写一万条数据性能===============~n"),
	{ok, DiscOnlyCopiesWriteTime, DiscOnlyCopiesReadTime} = tester:test_disc_only_copies_read_write(),
	io:format("disc_only_copies在已有1万条数据情况下: ~n"),
	io:format("写入一万条数据所消耗时间: ~p 微秒~n", [DiscOnlyCopiesWriteTime]),
	io:format("读取一万条数据所消耗时间: ~p 微秒~n~n~n", [DiscOnlyCopiesReadTime]),

	ok.

stop() ->
	application:stop(read_write_test).