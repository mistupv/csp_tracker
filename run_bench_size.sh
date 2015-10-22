#!/usr/bin/env escript
%% -*- erlang -*-
%%! +P 1000000 -smp enable -env ERL_MAX_ETS_TABLES 100000


main([String]) ->
	io:format("Process limit: ~p\n",[erlang:system_info(process_limit)]),
	csp_tracker_loader:load(),
	File = list_to_atom(String),  
	MiliSecList = lists:seq(53000, 60000, 1000),
	[csp_bench:bench_no_latex(File,'MAIN',MiliSec,50) 
	 || MiliSec <- MiliSecList],
	ok.