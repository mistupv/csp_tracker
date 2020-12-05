#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin +P 1000000 -smp enable -env ERL_MAX_ETS_TABLES 100000

main([String]) ->
  main([String,"1000"]);
main([String,Iterations]) ->
  main([String,Iterations,"2000"]);
main([String,Iterations,Timeout]) ->
	File = list_to_atom(String),
	csp_bench:bench(File,'MAIN',list_to_integer(Timeout),list_to_integer(Iterations)).
