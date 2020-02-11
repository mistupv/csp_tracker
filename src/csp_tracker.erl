%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing).
%   csptrack:track([]).
%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing),csptrack:track([]).
%   csptrack:track([only_externals]).

-module(csp_tracker).

-export([	
			track/1, track/2, track/3, 
			track_web/3, track_web_slice/1,
			rewrite_renamings/2, preprocess_variables/1,
			insert_processes/2, read_channels_info/1,
			build_digraph/2, print_from_digraph/4
		]).


-include("csp_tracker.hrl").

track(File) -> track(File,'MAIN', [all,infinity]).

track(File,FirstProcess) -> track(File,FirstProcess,[all,infinity]).

track(File,FirstProcess,Options) when is_atom(File) and is_list(Options) ->
	FunAnswer = 
		fun(Digraph, TotalSlice) when TotalSlice > 0 ->
				Answer = 
					get_answer(
						"\nWhich execution are you interested? ",
						lists:seq(1, TotalSlice) ),
				{Slice, Time} = get_slices_from_digraph(Digraph, Answer),
				io:format("\nTotal of time generating slice:\t~p ms\n",[Time/1000]),
				slice_from(Digraph, Slice),
				get_slice_code(Digraph, Slice, FirstProcess, File),
				% io:format("~p\n", [Lines]),
				ok;
			(_, _) -> 
				ok
		end,
	track_common(File, FirstProcess,Options, FunAnswer);
track(_,_,_) -> io:format("Not valids arguments.\n").

track_web(File,FirstProcess,Options) when is_atom(File) and is_list(Options) ->
	track_common(File, FirstProcess,Options, fun(_, _) -> ok end).

track_web_slice(Ex) ->
	try 
		File = 'csp_tracker_temp.csp',
		% File = 'ex1.csp',
		{ok,[NodesDigraph, EdgesDigraph]} = file:consult("track.txt"),
		Digraph = build_digraph(NodesDigraph, EdgesDigraph),
		{Slice, Time} = get_slices_from_digraph(Digraph, Ex),
		io:format("\nTotal of time generating slice:\t~p ms\n",[Time/1000]),
		slice_from(Digraph, Slice),
		 FirstProcess = 
	 	   case file:consult("first_process.txt") of
	 	   	{ok,[FirstProcess_]} -> 
	 	   		FirstProcess_;
	 	   	_ -> 
	 	   		'MAIN'
	 	   end,
		get_slice_code(Digraph, Slice, FirstProcess, File),
		ok
	catch 
		_:_ ->
			io:format("Unable to generate the slice\n")
	end.


track_common(File, FirstProcess,Options, FunAnswer) ->
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
			result_for_error();
		{ok,ProcessList} ->
			% io:format("~p\n", [ProcessList]),
			case ProcessList of
			     [[]|_] -> 
			     	io:format("Correct the syntax error before to proceed\n"),
			     	result_for_error();
			     [_|_] -> 
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
			%		          [[ PN || {PN,_} <- ets:tab2list(Processes)]--
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
					{{{N,E,S,TimeAfterExecuting},_G,Trace}, DigraphContent} = 
						csp_process:first(FirstProcess,Timeout,NoOutput),
					{NodesDigraph, EdgesDigraph} = DigraphContent,
					% TimeBeforeTrack = now(),
					Digraph = build_digraph(NodesDigraph, EdgesDigraph),
					% TimeAfterTrack = now(),
					% io:format("Total of time generate track:\t~p ms\n",[timer:now_diff(TimeAfterTrack - TimeBeforeTrack)]),
					%TimeAfterExecuting = now(),
					case Timeout of
						infinity -> 
							ok;
						_ -> 
							% printer:add_to_file(G,NoOutput),
							print_from_digraph(Digraph, "track", [], NoOutput),
							case NoOutput of 
								false ->
									io:format("\n*********** Trace ************\n\n~s\n******************************\n",[Trace]);
								true ->
									ok 
							end
					end,
					TimeExecuting = timer:now_diff(TimeAfterExecuting, TimeBeforeExecuting),
					SizeFile = filelib:file_size("track.dot"), 
					% io:format("~p.\n~p.\n", [
					% 	[digraph:vertex(Digraph, V)  || V <- digraph:vertices(Digraph)], 
					% 	[digraph:edge(Digraph, E)  || E <- digraph:edges(Digraph)]]),
					TrackStr = 
						io_lib:format("~p.\n~p.\n", [NodesDigraph, EdgesDigraph]),
					file:write_file("track.txt", list_to_binary(TrackStr)),
					[_,{memory,Words},_] = digraph:info(Digraph),
					Result1 = 
						case NoOutput of 
							false ->
								io:format("\n********** Results ************\n"),
								io:format("Total of time converting:\t~p ms\n",[TimeConversion/1000]),
								io:format("Total of time executing:\t~p ms\n",[TimeExecuting/1000]),
								% io:format("Total of time generate track:\t~p ms\n",[timer:now_diff(TimeAfterTrack, TimeBeforeTrack) / 1000]),
								io:format("Total of time:\t~p ms\n",[(TimeExecuting + TimeConversion)/1000]),
								io:format("Total of node:\t~p nodes\n",[N]),
								io:format("Total of control edges:\t~p edges\n",[E]),
								io:format("Total of synchronization edges:\t~p edges\n",[S]),
								io:format("Total of edges:\t~p edges\n",[E + S]),
								io:format("Size of DOT file:\t~p bytes\n",[SizeFile]),
								io:format("Track size in memory:\t~p bytes\n", [Words * erlang:system_info(wordsize)]),
								io:format("******************************\n");
							true ->
								{{N,E,S},TimeConversion,TimeExecuting,TimeConversion + TimeExecuting,SizeFile}
						end,
					DigraphComplete = build_digraph(NodesDigraph, EdgesDigraph),
					TotalSlice = csp_slicer:get_total_slices(DigraphComplete),
					Result2 = 
						case NoOutput of 
							false ->
								io:format("\n*********** Slice ************\n"),
								case TotalSlice of 
									0 ->
										io:format("Slicing criterion not executed.\n");
									_ ->
										io:format("The slicing criterion was executed " 
											++ integer_to_list(TotalSlice) ++ " times.\n"),
										FunAnswer(DigraphComplete, TotalSlice)
								end,
								io:format("*******************************\n"),
								io:format("~p\n", [TotalSlice]);
							true -> 
								case TotalSlice of 
									0 ->
										0;
									_ ->
										{Slice, TimeCal} = get_slices_from_digraph(DigraphComplete, 1),
										remove_slice_nodes(DigraphComplete),
										Lines = read_lines_file(File),
										{_, TimeGen} = slice_output(Slice, FirstProcess, DigraphComplete, Lines),
										TimeCal + TimeGen
								end
						end,
					csp_process:send_message2regprocess(codeserver, stop),
					{Result1, Result2};
				_ ->
					result_for_error()
			end
	end.

result_for_error() ->
	{{{0,0,0},0,0,0,0},0}.

get_slice_code(Digraph, Slice, FirstProcess, File) ->
	remove_slice_nodes(Digraph),
	Lines = read_lines_file(File),
	{{ResGap, ResExec}, Time} = slice_output(Slice, FirstProcess, Digraph, Lines),
	io:format("\n********* Gaps Slice **********\n\n"),
	io:format("~s\n", [ResGap]),
	io:format("*******************************\n"),
	io:format("\n******* Executable Slice ******\n\n"),
	io:format("~s\n", [ResExec]),
	io:format("*******************************\n"),
	io:format("Total of time creating output:\t~p ms\n",[Time/1000]).

get_slices_from_digraph(Digraph, Ex) ->
	TimeBeforeExecuting = now(),
	Slice = csp_slicer:get_slices(Digraph, Ex),
	TimeExecuting = timer:now_diff(now(), TimeBeforeExecuting),
	{Slice,TimeExecuting}.

slice_from(Digraph, Slice) ->
	print_from_digraph(Digraph, "track_slice", Slice, false).
	

print_from_digraph(Digraph, NameFile, Slice, NoOutput) ->
	remove_slice_nodes(Digraph),
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
	file:write_file(NameFile ++ ".dot", 
		list_to_binary("digraph " ++ NameFile ++ " {\n" ++
			"rankdir = LR;\n" ++ NodesSlice ++ EdgesSlice ++ "\n}")),
	case NoOutput of 
		true -> 
			ok;
		false ->  
			PdfOutputFun = 
				fun()->
					PidParent = 
						receive 
							{pdf_pid, PidParent_} ->
								PidParent_
						end,
					os:cmd("dot -Tpdf " ++ NameFile ++ ".dot > " ++ NameFile ++ ".pdf"),
					PidParent!pdf_finished
				end,
			PidPdfProcess = spawn(PdfOutputFun),
			PidPdfProcess!{pdf_pid, self()},
			receive 
				pdf_finished ->
					ok
			after 
				2000 ->
					try
						exit(PidPdfProcess, ok),
						os:cmd("pkill dot")
					catch
						_:_ ->
							ok
					end
					% io:format("\nUnable to generate PDF output.\n")
			end
	end.
	% ok.

slice_output(Slice, FirstProcess, G, Lines) ->
	TimeBeforeExecuting = now(),
	Output = csp_slicer_output:create_slicer_output(Slice, FirstProcess, G, Lines),
	TimeExecuting = timer:now_diff(now(), TimeBeforeExecuting),
	{Output, TimeExecuting}.

remove_slice_nodes(Digraph) ->
	[begin 
		case digraph:vertex(Digraph, VD) of 
			false -> 
				ok;
			{Id, {Label, _}} -> 
				case Label of 
					?SLICE -> 
						OsArrow = digraph:out_edges(Digraph, Id),
						InfoOsArrow = [ digraph:edge(Digraph, O) || O <- OsArrow ],
						VArrow = hd(lists:usort([VArrow_ || {_EArrow,_,VArrow_,"control"} <- InfoOsArrow])),
						Es = digraph:in_edges(Digraph, Id),
						InfoEs = [ digraph:edge(Digraph, E) || E <- Es ],
						Os = digraph:out_edges(Digraph, VArrow),
						InfoOs = [ digraph:edge(Digraph, O) || O <- Os ],
						[[ digraph:add_edge(Digraph, V1, V2, "control") || {_,_,V2,_} <- InfoOs] 
							|| {_,V1,_,_} <- InfoEs],
						digraph:del_vertex(Digraph, Id),
						digraph:del_vertex(Digraph, VArrow);
					_ -> 
						ok
				end
		end
	end || VD <- digraph:vertices(Digraph)].

build_digraph(NodesDigraph, EdgesDigraph) ->
	Digraph = digraph:new(),
	[ digraph:add_vertex(Digraph, V, Label) 
		|| {V, Label} <- NodesDigraph],
	[ digraph:add_edge(Digraph, V1, V2, Label)  
		|| {V1, V2, Label} <- EdgesDigraph],
	Digraph.

insert_processes([{}],_) ->
	ok;
insert_processes([{ProcessName0, ProcessBody, SPAN}|Tail],Processes) ->
	%NProcessBody = csp_parsing:replace_fake_processes(ProcessBody,Processes),
	{ProcessName,Parameters} = 
		csp_parsing:search_parameters(atom_to_list(ProcessName0),[]),
	%io:format("Name: ~p  Parameters: ~p\n",[ProcessName,Parameters]),
	ets:insert(Processes,{ProcessName,{{Parameters,ProcessBody},SPAN}}),
	insert_processes(Tail,Processes);
insert_processes([{Channel,Type}|Tail],Processes) ->
	ets:insert(Processes,{list_to_atom(Channel),Type}),
	insert_processes(Tail,Processes);
insert_processes([],_) ->
	ok.
	
preprocess_variables() ->
	preprocess_variables("").

preprocess_variables(Dir) ->
	{ok, IODevice} = file:open(Dir ++ "output.txt",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Rewritten = csp_parsing:rewrite_vars(Read),
	file:write_file(Dir ++ "output_rewritten.txt", list_to_binary(Rewritten)).

rewrite_renamings(File) ->
	rewrite_renamings(File, "output1.csp").

rewrite_renamings(File, FileOut) ->
	{ok, IODevice} = file:open(File,[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Rewritten = separate_renamings(Read),
	file:write_file(FileOut, list_to_binary(Rewritten)).
	
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
	     Data -> read_file(IODevice, Acc ++ Data)
	end.

read_channels_info() ->	
	read_channels_info("").
	
read_channels_info(Dir) ->
	{ok, IODevice} = file:open(Dir ++ "output.pl",[read]),
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

% read_lines_file(IODevice) -> 
% 	read_lines_file(IODevice, 1, []).
	
% read_lines_file(IODevice, Line, Acc) ->
% 	case io:request(IODevice, {get_line, ''}) of
% 	     eof -> 
% 	     	Acc;
% 	     Data -> 
% 	     	read_lines_file(IODevice, Line + 1, Acc ++ [{Line,Data}])
% 	end.
	
read_lines_file( File ) ->
        {ok, IO} = file:open( File, [read] ),
        read_lines_file( io:get_line(IO, ''), IO, 1, [] ).
 
 
read_lines_file( eof, _IO, _Num, Acc ) -> lists:reverse( Acc );
read_lines_file( {error, _Error}, _IO, _Num, Acc ) -> lists:reverse( Acc );
read_lines_file( Line, IO, Num, Acc ) -> read_lines_file( io:get_line(IO, ''), IO, Num + 1, [{Num,Line} | Acc] ).


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