-module(codeserver).

-export([loop/1]).


loop(Processes) ->
	random:seed(now()),
	receive
		{ask_code,Process,Arguments,Pid} -> 
			AllProcesses =  ets:lookup(Processes, Process),
			%io:format("~p\n",[AllProcesses]),
			% io:format("~p\n",[get_body(Process,Arguments,AllProcesses)]),
			{PBody, _SPAN} = get_body(Process,Arguments,AllProcesses),
			Pid!{code_reply,PBody},
			loop(Processes);
		{ask_span,Process,Arguments,Pid} -> 
			AllProcesses =  ets:lookup(Processes, Process),
			%io:format("~p\n",[AllProcesses]),
			{_PBody, SPAN} = get_body(Process,Arguments,AllProcesses),
			Pid!{code_reply,SPAN},
			loop(Processes);
		{ask_channel,Channel,Pid} -> 
			{Channel,Types} =  hd(ets:lookup(Processes, Channel)),
			SelectedChannels = 
				[begin lists:nth(random:uniform(length(T)),T) end|| T <- Types],
			% io:format("Channel: ~p\nTypes: ~p\nSelectedChannels: ~p\n",[Channel,Types, SelectedChannels]),
			Pid!{channel_reply,SelectedChannels},
			loop(Processes);
		stop -> 
			ok
	end.

get_body(Process,Arguments,[{Process,{{Parameters,PBody}, SPAN}}|Tail]) ->
	case csp_parsing:matching(Parameters,Arguments) of
	     true ->
	     	{csp_parsing:replace_parameters(PBody,lists:zip(Parameters,Arguments)), SPAN};
	     	% Result = csp_parsing:replace_parameters(PBody,lists:zip(Parameters,Arguments)),
	     	% io:format("Body: ~p\n",[Result]),
	     	% io:format("ParArgs: ~p\n",[lists:zip(Parameters,Arguments)]),
	     	% Result;
	     false ->
	     	get_body(Process,Arguments,Tail)
	end;
get_body(Process,Arguments,[_|Tail]) ->
	get_body(Process,Arguments,Tail);
get_body(_,_,[]) ->
	%Açi deuria mostrar un error i parar
	{{stop,{src_span,0,0,0,0,0,0}}, {src_span,0,0,0,0,0,0}}.




