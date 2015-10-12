-module(csp_bench).

-export([bench/4, bench_no_latex/4]).

bench(File,InitialProcees,Timeout,Iterations) ->
	csp_tracker:track(File,InitialProcees,[Timeout,no_output]),
	Result = bench_aux(File,InitialProcees,Timeout,Iterations,Iterations,[[],[],[],[],[],[],[],[]]),
	FunOutput = 
		fun(Means, C001, C005) ->
			io:format("Data for alpha = 0,01\n"),
			printLatex(C001,Means),
			io:format("Data for alpha = 0,05\n"),
			printLatex(C005,Means)
		end,
	io:format("Data for arithmetic means.\n"),
	calculate_with_mean(Result,Iterations,arithmetic, FunOutput),
	io:format("Data for harmonic means.\n"),
	calculate_with_mean(Result,Iterations,harmonic, FunOutput).

bench_no_latex(File,InitialProcees,Timeout,Iterations) ->
	csp_tracker:track(File,InitialProcees,[Timeout,no_output]),
	Result = bench_aux(File,InitialProcees,Timeout,Iterations,Iterations,[[],[],[],[],[],[],[],[]]),
	% io:format("Data for arithmetic means.\n"),
	FunOutput = 
		fun(Means, _, _) ->
			io:format("~p ~p\n", [Timeout, lists:nth(7,Means)])
		end,
	calculate_with_mean(Result,Iterations,arithmetic, FunOutput).


calculate_with_mean(Result,Iterations,TypeMean, FunOutput) ->
	Means = 
		case TypeMean of 
			arithmetic -> 
				[lists:sum(R)/ Iterations || R <- Result];
			harmonic ->
				[Iterations/lists:sum([(1/X) || X <- R])|| R <- Result]
		end,
	ResultMeans = lists:zip(Result,Means),
	% io:format("~p\n",[ResultMeans]),
	StandardDeviations = 
		[math:sqrt(lists:sum([(R - M) * (R - M) || R <- Rs]) / (Iterations - 1)) 
			|| {Rs,M} <- ResultMeans],
	MeansStdDev = lists:zip(Means,StandardDeviations),
	FunCalculateCs = 
		fun(Z) ->
			[begin 
				Z_S = Z * (S / math:sqrt(Iterations)),
				{case (Left = M - Z_S) < 0 of 
					true -> 0.0;
					false -> Left 
				 end, 
				 M + Z_S} 
			end || {M,S} <- MeansStdDev]
		end,
	C005 = FunCalculateCs(1.96),
	C001 = FunCalculateCs(2.575),
	% io:format("~p\n",[{Means,StandardDeviations,C005,C001}]),
	FunOutput(Means, C001, C005).


bench_aux(_,_,_,_,0,Acc) ->
	Acc;
bench_aux(File,InitialProcees,Timeout,TotalIterations,Iterations,[TN,TCE,TSE,TTC,TTE,TTT,TSF,TSLC]) ->
	Result = csp_tracker:track(File,InitialProcees,[Timeout,no_output]),
	% io:format("Iteration ~p: ~p\n",[1 + TotalIterations - Iterations, Result]),
	{{{IN,ICE,ISE},ITC,ITE,ITT,ISF},ITSLC} = Result,
	case Result of 
		{{{0,0,0},_,_,_,_},_} ->
			bench_aux(File,InitialProcees,Timeout,TotalIterations,Iterations,
				[TN,TCE,TSE,TTC,TTE,TTT,TSF]);
		_ ->
			case lists:member(0,[IN,ICE,ISE,ITC,ITE,ITT,ISF]) of 
				true -> 
					bench_aux(File,InitialProcees,Timeout,TotalIterations,Iterations,
						[TN,TCE,TSE,TTC,TTE,TTT,TSF,TSLC]);
				false ->
					bench_aux(File,InitialProcees,Timeout,TotalIterations,Iterations - 1,
						[[IN|TN],[ICE|TCE],[ISE|TSE],[ITC|TTC],[ITE|TTE],[ITT|TTT],[ISF|TSF],[ITSLC|TSLC]])
			end
	end. 


getListCI(N,C,Means) ->
	[element(1,lists:nth(N,C)), lists:nth(N,Means), element(2,lists:nth(N,C))].

printLatex(C,Means) ->
	Times_ = getListCI(4,C,Means) ++ getListCI(5,C,Means) ++ getListCI(6,C,Means) ++ getListCI(7,C,Means),
	Times = [T/1000 || T <- Times_],
	Graph = getListCI(1,C,Means) ++ getListCI(2,C,Means) ++ getListCI(3,C,Means) ,
	Slice = getListCI(8,C,Means),
	io:format("& $[_{~.2f}~~~.2f~~_{~.2f}]$	& $[_{~.2f}~~~.2f~~_{~.2f}]$ & $[_{~.2f}~~~.2f~~_{~.2f}]$ \\\\~n",Times),
	io:format("& $[_{~.2f}~~~.2f~~_{~.2f}]$	& $[_{~.2f}~~~.2f~~_{~.2f}]$ & $[_{~.2f}~~~.2f~~_{~.2f}]$ & $[_{~.2f}~~~.2f~~_{~.2f}]$ \\\\~n",Graph),
	io:format("& $[_{~.2f}~~~.2f~~_{~.2f}]$	\\\\~n",Slice).
