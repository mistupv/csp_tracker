-module(codeserver).

-export([loop/1]).


loop(Processes) ->
	receive
		{ask_code,Process,Arguments,Pid} -> 
			AllProcesses =  ets:lookup(Processes, Process),
			%io:format("~p\n",[AllProcesses]),
			PBody = get_body(Process,Arguments,AllProcesses),
			Pid!{code_reply,PBody},
			loop(Processes);
		stop -> 
			ok
	end.

get_body(Process,Arguments,[{Process,{Parameters,PBody}}|Tail]) ->
	case csp_parsing:matching(Parameters,Arguments) of
	     true ->
	     	csp_parsing:replace_parameters(PBody,lists:zip(Parameters,Arguments));
	     false ->
	     	get_body(Process,Arguments,Tail)
	end;
get_body(Process,Arguments,[_|Tail]) ->
	get_body(Process,Arguments,Tail);
get_body(_,_,[]) ->
	%Açi deuria mostrar un error i parar
	{stop,{src_span,0,0,0,0,0,0}}.


