%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing).
%   csptrack:track([]).
%   c(csptrack),c(codeserver),c(printer),c(csp_process),c(csp_parsing),csptrack:track([]).
%   csptrack:track([only_externals]).
-module(csp_tracker).

-export([track/1, track/2]).


track(File) -> track(File,[all,infinity]).

track(File,Options) when is_atom(File) and is_list(Options)->
	io:format(os:cmd("./createoutput.sh "++atom_to_list(File))),
	preprocess_variables(),
	{ok,ProcessList} = file:consult("output_rewritten.txt"),
	case hd(ProcessList) of
	     [] -> 
	     	io:format("Correct the syntax error before to proceed\n"),
	     	ok;
	     _ -> 
		file:write_file("track.dot", list_to_binary("digraph csp_track {\n}")),
		Processes = ets:new(processes,[bag]),
		insert_processes(hd(ProcessList),Processes),
%		io:format("Processes: ~p\n",
%		          [[ PN || {PN,_} <- ets:tab2list(Processes)]--
%		           [ PN || {PN,_} <- ets:tab2list(Processes),
%		                   csp_parsing:fake_process_name(atom_to_list(PN))]]),
		case lists:member(codeserver,registered()) of
		     true -> ok;
		     false -> 
		     	register(codeserver, spawn(codeserver,loop,[Processes]))
		end,
		case lists:member(printer,registered()) of
		     true -> ok;
		     false -> 
		     	register(printer, 
		         spawn(printer,loop,
		               [case lists:member(only_externals,Options) of
				     true -> only_externals;
				     false -> all
		     		end]))
		end,					
		Timeout = 
		  case [Opt || Opt <- Options, is_number(Opt)] of
		       [TO|_] -> TO;
		       _ -> infinity
		  end,
		%io:format("Timout: ~p\n",[Timeout]),
		csp_process:first(Timeout)
	end;
track(_,_) -> io:format("Not valids arguments.\n").

insert_processes([{}],_) ->
	ok;
insert_processes([{ProcessName0,ProcessBody,_}|Tail],Processes) ->
	%NProcessBody = csp_parsing:replace_fake_processes(ProcessBody,Processes),
	{ProcessName,Parameters} = csp_parsing:search_parameters(atom_to_list(ProcessName0),[]),
	%io:format("Name: ~p  Parameters: ~p\n",[ProcessName,Parameters]),
	ets:insert(Processes,{ProcessName,{Parameters,ProcessBody}}),
	insert_processes(Tail,Processes);
insert_processes([],_) ->
	ok.
	
preprocess_variables() ->
	{ok, IODevice} = file:open("output.txt",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	Rewritten = csp_parsing:rewrite_vars(Read),
	file:write_file("output_rewritten.txt", list_to_binary(Rewritten)).
	
read_file(IODevice) -> 
	read_file(IODevice,[]).
	
read_file(IODevice,Acc) ->
	case io:request(IODevice, {get_line, ''}) of
	     eof -> Acc;
	     Data -> read_file(IODevice,Acc++Data)
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