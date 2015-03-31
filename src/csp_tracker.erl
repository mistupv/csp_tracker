%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing).
%   csptrack:track([]).
%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing),csptrack:track([]).
%   csptrack:track([only_externals]).

-module(csp_tracker).

-export([track/1, track/2, track/3]).


track(File) -> track(File,'MAIN', [all,infinity]).

track(File,FirstProcess) -> track(File,FirstProcess,[all,infinity]).

track(File,FirstProcess,Options) when is_atom(File) and is_list(Options) ->
	rewrite_renamings(atom_to_list(File)),
	NoOutput = lists:member(no_output,Options),
	TimeBeforeConversion = now(),
	OutputConversion = 
		os:cmd("./createoutput.sh output1.csp"),
	preprocess_variables(),
	TimeAfterConversion = now(),
	case NoOutput of 
		false -> io:format("~s\n",[OutputConversion]);
		true -> ok
	end,
	TimeConversion = timer:now_diff(TimeAfterConversion, TimeBeforeConversion),
	% case file:consult("output_rewritten1.txt") of
	case file:consult("output_rewritten.txt") of
		{error,{_,_,InfoError}} ->
			io:format("Error reading Erlang translation:\n~s\n",[lists:flatten(erl_parse:format_error(InfoError))]),
			io:format("Correct the syntax error before to proceed\n"),
			ok;
		{ok,ProcessList} ->
			case hd(ProcessList) of
			     [] -> 
			     	io:format("Correct the syntax error before to proceed\n"),
			     	ok;
			     _ -> 
					file:write_file("track.dot", list_to_binary("digraph csp_track {\n}")),
					Processes = ets:new(processes,[bag]),
					insert_processes(hd(ProcessList),Processes),
					ChannelInfo_ = read_channels_info(),
					ChannelInfo = 
						[{Channel, csp_parsing:extract_type(Type)} 
						 || {Channel, Type} <- ChannelInfo_],
					% io:format("~p\n",[ChannelInfo]),
					insert_processes(ChannelInfo,Processes),
			%		io:format("Processes: ~p\n",
			%		          [[ PN || {PN,_} <- ets:tab2list(Processes)]--
			%		           [ PN || {PN,_} <- ets:tab2list(Processes),
			%		                   csp_parsing:fake_process_name(atom_to_list(PN))]]),
					case lists:member(codeserver,registered()) of
					     true -> ok;
					     false -> 
					     	register(codeserver, spawn(codeserver,loop,[Processes]))
					end,
					Timeout = 
					  case [Opt || Opt <- Options, is_number(Opt)] of
					       [TO|_] -> TO;
					       _ -> infinity
					  end,
					case lists:member(printer,registered()) of
					     true -> ok;
					     false -> 
					     	register(printer, 
					         spawn(printer,loop,
					            [case lists:member(only_externals,Options) of
							     true -> only_externals;
							     false -> all
					     		end,
					     		case Timeout of
							     infinity -> true;
							     _ -> false
					     		end]))
					end,					
					%io:format("Timout: ~p\n",[Timeout]),
					TimeBeforeExecuting = now(),
					{{{N,E,S,TimeAfterExecuting},G,Trace}, DigraphContent} = csp_process:first(FirstProcess,Timeout,NoOutput),
					%TimeAfterExecuting = now(),
					case Timeout of
						infinity -> 
							ok;
						_ -> 
							printer:add_to_file(G,NoOutput),
							case NoOutput of 
								false ->
									io:format("\n************Trace*************\n\n~s\n",[Trace]);
								true ->
									ok 
							end
					end,
					TimeExecuting = timer:now_diff(TimeAfterExecuting, TimeBeforeExecuting),
					SizeFile = filelib:file_size("track.dot"), 
					{NodesDigraph, EdgesDigraph} = DigraphContent,
					Digraph = digraph:new(),
					[ digraph:add_vertex(Digraph, V, Label) 
						|| {V, Label} <- NodesDigraph],
					[ digraph:add_edge(Digraph, V1, V2, Label)  
						|| {V1, V2, Label} <- EdgesDigraph],
					% io:format("~p\n~p\n", [
					% 	[digraph:vertex(Digraph, V)  || V <- digraph:vertices(Digraph)], 
					% 	[digraph:edge(Digraph, E)  || E <- digraph:edges(Digraph)]]),
					io:format("\n******************************\n"),
					TotalSlice = csp_slicer:get_total_slices(Digraph),
					case TotalSlice of 
						0 ->
							io:format("Slice not executed.\n");
						_ ->
							Answer = 
								get_answer(
									"Total of executions is " ++ integer_to_list(TotalSlice) 
										++ ".\nIn which one are you interested? ",
									lists:seq(1, TotalSlice) ),
							Slice = csp_slicer:get_slices(Digraph, Answer),
							NodesSlice = 
								lists:flatten([
									begin 
										{Id, {Label, SPAN}} = digraph:vertex(Digraph, VD),
										printer:string_vertex_dot(Id, Label, SPAN, Slice)
									end || VD <- digraph:vertices(Digraph)]),
							EdgesSlice = 
								lists:flatten([
									begin 
										{_, V1, V2, Type} = digraph:edge(Digraph, ED),
										printer:string_edge_dot(V1, V2, Type)
									end || ED <- digraph:edges(Digraph)]),
							file:write_file("track_slice.dot", 
								list_to_binary("digraph csp_track_slice {" ++ NodesSlice ++ EdgesSlice ++ "\n}")),
							os:cmd("dot -Tpdf track_slice.dot > track_slice.pdf")
					end,
					io:format("******************************\n"),
					case NoOutput of 
						false ->
							io:format("\n******************************\n"),
							io:format("Total of time converting:\t~p ms\n",[TimeConversion/1000]),
							io:format("Total of time executing:\t~p ms\n",[TimeExecuting/1000]),
							io:format("Total of time:\t~p ms\n",[(TimeExecuting + TimeConversion)/1000]),
							io:format("Total of node:\t~p nodes\n",[N]),
							io:format("Total of control edges:\t~p edges\n",[E]),
							io:format("Total of synchronization edges:\t~p edges\n",[S]),
							io:format("Total of edges:\t~p edges\n",[E + S]),
							io:format("Size of DOT file:\t~p bytes\n",[SizeFile]),
							io:format("******************************\n");
						true ->
							{{N,E,S},TimeConversion,TimeExecuting,TimeConversion + TimeExecuting,SizeFile}
					end
			end
	end;
track(_,_,_) -> io:format("Not valids arguments.\n").

insert_processes([{}],_) ->
	ok;
insert_processes([{ProcessName0,ProcessBody,_}|Tail],Processes) ->
	%NProcessBody = csp_parsing:replace_fake_processes(ProcessBody,Processes),
	{ProcessName,Parameters} = 
		csp_parsing:search_parameters(atom_to_list(ProcessName0),[]),
	%io:format("Name: ~p  Parameters: ~p\n",[ProcessName,Parameters]),
	ets:insert(Processes,{ProcessName,{Parameters,ProcessBody}}),
	insert_processes(Tail,Processes);
insert_processes([{Channel,Type}|Tail],Processes) ->
	ets:insert(Processes,{list_to_atom(Channel),Type}),
	insert_processes(Tail,Processes);
insert_processes([],_) ->
	ok.
	
preprocess_variables() ->
	{ok, IODevice} = file:open("output.txt",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Rewritten = csp_parsing:rewrite_vars(Read),
	file:write_file("output_rewritten.txt", list_to_binary(Rewritten)).

rewrite_renamings(File) ->
	{ok, IODevice} = file:open(File,[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Rewritten = separate_renamings(Read),
	file:write_file("output1.csp", list_to_binary(Rewritten)).
	
separate_renamings([$[,$[|Tail]) ->
	{SeparatedRenaming,NTail} = read_until_brackets(Tail,"[["),
	SeparatedRenaming ++ separate_renamings(NTail);
separate_renamings([Char|Tail]) ->
	[Char|separate_renamings(Tail)];
separate_renamings([]) ->
	[].

read_until_brackets([$,|Tail],Acc) ->
	{NAcc,NTail} = read_until_brackets(Tail,"[["),
	{Acc ++ "]]" ++ NAcc,NTail};
read_until_brackets([$],$]|Tail],Acc) ->
	{Acc ++ "]]",Tail};
read_until_brackets([$ |Tail],Acc) ->
	read_until_brackets(Tail,Acc);
read_until_brackets([Char|Tail],Acc) ->
	read_until_brackets(Tail,Acc ++ [Char]);
read_until_brackets([],_) ->
	throw({error,"Not correct syntax. A renaming is not closed.",{}}).

read_file(IODevice) -> 
	read_file(IODevice,[]).
	
read_file(IODevice,Acc) ->
	case io:request(IODevice, {get_line, ''}) of
	     eof -> Acc;
	     Data -> read_file(IODevice,Acc++Data)
	end.
	
read_channels_info() ->
	{ok, IODevice} = file:open("output.pl",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Lines = string:tokens(Read, "\n"),
	get_channels_info(Lines).

get_channels_info([[$',$c,$h,$a,$n,$n,$e,$l,$',$(,$' | Tail]|Pending]) ->
	Searched = "','type'(",
	PositionToCut = string:str(Tail, Searched),
	Name = string:sub_string(Tail, 1, PositionToCut - 1),
	Type = string:sub_string(Tail, PositionToCut + length(Searched), length(Tail) - 3),
	[{Name,Type} | get_channels_info(Pending)];
get_channels_info([_|Pending]) ->
	get_channels_info(Pending);
get_channels_info([]) ->
	[].

get_answer(Message,Answers) ->
	[_|Answer] = 
		lists:reverse(io:get_line(Message)),
	try
		IntegerAnswer = list_to_integer(lists:reverse(Answer)),
		case lists:member(IntegerAnswer,Answers) of
		    true -> IntegerAnswer;
		    false -> get_answer(Message,Answers)
		end
	catch 
		_:_ -> get_answer(Message,Answers)
	end.


%Prova per veure que pasa quant varios processos intenten matar-se mutuament.	
%a(N) ->
%	Pids = create_killers(N),
%	[Pid!{pids,Pids--[Pid]}||Pid <- Pids],
%	ok.
%	
%create_killers(0) -> [];
%create_killers(N) -> 
%	[spawn(fun() -> killer() end)|create_killers(N-1)].
%	
%killer() ->
%	Pids = 
%		receive
%			{pids,Pids_} -> Pids_
%		end,
%	receive
%	after 250 -> ok
%	end,
%	io:format("~p va a matar\n",[self()]),
%	[exit(Pid, kill)||Pid <- Pids],
%	io:format("Sobrevive: ~p\n",[self()]).