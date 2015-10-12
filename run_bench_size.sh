#!/usr/bin/env escript
%% -*- erlang -*-
%%! +P 1000000 -smp enable -env ERL_MAX_ETS_TABLES 100000


main([String]) ->
	io:format("Process limit: ~p\n",[erlang:system_info(process_limit)]),
	csp_tracker_loader:load(),
	File = list_to_atom(String),  
	% MiliSecList = lists:seq(100, 5000, 100),
	MiliSecList = lists:seq(500, 20000, 500),
	[csp_bench:bench_no_latex(File,'MAIN',MiliSec,10) 
	 || MiliSec <- MiliSecList],
	ok.