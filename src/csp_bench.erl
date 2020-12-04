-module(csp_bench).

-export([bench/4]).

bench(File,InitialProcess,Timeout,Iterations) ->
	io:format("tracker mode: ~p~n", [csp_util:tracker_mode()]),
	load_beams(),
	bench_aux(File,InitialProcess,Timeout,Iterations,Iterations).


%% Meaning of accumulated [T...] and next [I...] results obtained in the benchmark
%%   {{{N,CE,SE},TC,TE,TT,SF},SLC} = Result
%% 	N  = number of nodes,
%%  CE = number of control edges,
%%  SE = number of sync edges,
%%  TC = time conversion csp -> erl,
%%  TE = time executing,
%%  TT = time conv + exec,
%%  SF = SizeFile
%%  SLC = time to slice (generate and print the slice)

bench_aux(_,_,_,_,0) ->
	ok;
bench_aux(File,InitialProcess,Timeout,TotalIterations,Iterations) ->
	Result = csp_tracker:track(File,InitialProcess,[Timeout,no_output]),
	{{{_,_,_},_,ExecTimeMicro,_,_},_,FinishReason,Steps,MaxMemory} = Result,
	io:format("Iteration ~p (~p): ~p ~p ~p\n",[1 + TotalIterations - Iterations,
		FinishReason, Steps, ExecTimeMicro, MaxMemory]),
	case ExecTimeMicro of
		0 ->
			bench_aux(File,InitialProcess,Timeout,TotalIterations,Iterations);
		_ ->
			bench_aux(File,InitialProcess,Timeout,TotalIterations,Iterations - 1)
	end.

load_beams() ->
	code:ensure_modules_loaded([
		'csp_tracker',
		'csp_process',
		'csp_parsing',
		'csp_util',
		'printer',
		'codeserver'
	]).
