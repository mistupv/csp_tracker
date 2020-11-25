-module(codeserver).

-export([
	%% codeserver process API
	start_and_register_once/1,
	ask_code/2,
	ask_channel/1,
	%% spawn points
	loop/1
]).

%% PUBLIC API %%
start_and_register_once(Processes) ->
	case lists:member(printer,registered()) of
		true -> ok;
		false -> register(codeserver, fun() -> loop(Processes) end)
	end.

ask_code(ProcessName, Arguments) ->
	csp_util:send_message(codeserver, {ask_code, ProcessName, Arguments, self()}),
	receive
		{code_reply, Code} -> Code
	after 1000 -> throw(timeout)
	end.

ask_channel(Event) ->
	csp_util:send_message(codeserver, {ask_channel, Event, self()}),
	receive
		{channel_reply, Channels} -> Channels
	after 1000 -> throw(timeout)
	end.

%% PROCESS API %%

loop(Processes) ->
	receive
		{ask_code,Process,Arguments,Pid} -> 
			AllProcesses =  ets:lookup(Processes, Process),
			{PBody, _SPAN} = get_body(Process,Arguments,AllProcesses),
			Pid!{code_reply,PBody},
			loop(Processes);
		{ask_span,Process,Arguments,Pid} -> 
			AllProcesses =  ets:lookup(Processes, Process),
			{_PBody, SPAN} = get_body(Process,Arguments,AllProcesses),
			Pid!{code_reply,SPAN},
			loop(Processes);
		{ask_channel,Channel,Pid} -> 
			{Channel,Types} =  hd(ets:lookup(Processes, Channel)),
			SelectedChannels = [begin lists:nth(rand:uniform(length(T)),T) end|| T <- Types],
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




