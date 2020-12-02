#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin +P 1000000 -smp enable -env ERL_MAX_ETS_TABLES 100000

main([File]) ->
  main([File,"100"])
main([File,Iterations]) ->
	io:format("Process limit: ~p\n",[erlang:system_info(process_limit)]),
	MiliSecList = lists:seq(100, 5000, 100),
	[csp_bench:bench_no_latex(list_to_atom(File),'MAIN',MiliSec,list_to_integer(Iterations))
	 || MiliSec <- MiliSecList].
