-module(csp_slicer_output).

-export([create_slicer_output/3]).

-include("csp_tracker.hrl").

create_slicer_output(Slice, FirstProcess, G) ->
	Body = ask_code(FirstProcess,[]),
	SPANS = lists:usort(extract_spans(Slice, G)),
	io_lib:format("~p\n", [SPANS]),
	create_output_for_processes([{FirstProcess,Body}],SPANS, []).

create_output_for_processes([{Lhs,Rhs}|Tail], Slice, Done) ->
	case lists:member(Lhs, Done) of 
		false ->
			LhsStr = io_lib:format("~p", [Lhs]), 
			{Gaps, NewProcesses} = create_output(Rhs, Slice, ?NO_DEFINED),
			{Exec, _} = create_output(Rhs, Slice, ?EMPTY),
			{GapsT, ExecT} = create_output_for_processes(Tail ++ NewProcesses, Slice, [Lhs|Done]),
			BuildProc = fun(Str, StrOthers) -> LhsStr ++ " = " ++ Str ++ "\n\n" ++ StrOthers end,
			{BuildProc(Gaps, GapsT), BuildProc(Exec, ExecT)};
		true ->
			create_output_for_processes(Tail, Slice, Done)
	end;
create_output_for_processes([], _, _) ->
	{"", ""}.

create_output({prefix,SPANevent,Channels,Event,P,_SPANarrow},Slice, FillStr) ->
	case lists:member(SPANevent, Slice) of 
		true -> 
			{PStr, Proc} = create_output(P,Slice, FillStr),
			AllStr = 
				atom_to_list(Event) ++ printer:string_channels(Channels) ++ " -> " ++ PStr,
			{AllStr, Proc};
		false -> 
			{FillStr, []}
	end;
create_output({'|~|',P1,P2,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice) of 
		true -> 
			{P1Str, Proc1} = create_output(P1,Slice, FillStr),
			{P2Str, Proc2} = create_output(P2,Slice, FillStr),
			AllStr = P1Str ++ " |~| " ++ P2Str,
			{AllStr, Proc1 ++ Proc2};
		false -> 
			{FillStr, []}
	end;
create_output({'[]',P1,P2,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice) of 
		true -> 
			{P1Str, Proc1} = create_output(P1,Slice, FillStr),
			{P2Str, Proc2} = create_output(P2,Slice, FillStr),
			AllStr = P1Str ++ " [] " ++ P2Str,
			{AllStr, Proc1 ++ Proc2};
		false -> 
			{FillStr, []}
	end;
create_output({'ifte',Condition,P1,P2,SPAN1,_,_},Slice, FillStr) ->
	case lists:member(SPAN1, Slice) of 
		true -> 
			{P1Str, Proc1} = create_output(P1,Slice, FillStr),
			{P2Str, Proc2} = create_output(P2,Slice, FillStr),
			AllStr = P1Str ++ " << " ++ atom_to_list(Condition) ++ " >> " ++ P2Str,
			{AllStr, Proc1 ++ Proc2};
		false -> 
			{FillStr, []}
	end;
create_output({agent_call,SPAN,ProcessName,Arguments},Slice, FillStr) ->
	case lists:member(SPAN, Slice) of
		true ->
	        % case csp_parsing:fake_process_name(atom_to_list(ProcessName)) of
	        %      true -> 
	        %      	{"", [ProcessName]};
	        %      false ->
	        		io:format("Arguments: ~p\n",[Arguments]),
	                {atom_to_list(ProcessName) ++ printer:string_arguments(Arguments), 
	                 [{ProcessName, ask_code(ProcessName, Arguments)}]};
	        % end;
        false -> 
			{FillStr, []}
    end;
create_output({sharing,{closure,Events},P1,P2,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice) of 
		true -> 
			{P1Str, Proc1} = create_output(P1,Slice, FillStr),
			{P2Str, Proc2} = create_output(P2,Slice, FillStr),
			AllStr = P1Str ++ " [|{| "++ printer:string_list(Events) ++ " |}|] "++ P2Str,
			{AllStr, Proc1 ++ Proc2};
		false -> 
			{FillStr, []}
	end;
create_output({'|||',P1,P2,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice) of 
		true -> 
			{P1Str, Proc1} = create_output(P1,Slice, FillStr),
			{P2Str, Proc2} = create_output(P2,Slice, FillStr),
			AllStr = P1Str ++ " ||| " ++ P2Str,
			{AllStr, Proc1 ++ Proc2};
		false -> 
			{FillStr, []}
	end;
create_output({skip,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice, FillStr) of 
		true -> 
			{"SKIP", []};
		false -> 
			{FillStr, []}
	end;
create_output({stop,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice, FillStr) of 
		true -> 
			{"STOP", []};
		false -> 
			{FillStr, []}
	end;
create_output({procRenaming,Renamings,_,SPAN},Slice, FillStr) ->
	case lists:member(SPAN, Slice, FillStr) of 
		true -> 
			StringRenamingList0 =
				lists:flatten(
					[atom_to_list(Original) ++ " <- " ++ atom_to_list(Renamed) ++ "," 
					 || {rename,Original,Renamed} <- Renamings]),
			StringRenamingList = 
				lists:reverse(tl(lists:reverse(StringRenamingList0))),
			StringRenaming = "[[" ++ StringRenamingList ++ "]]",
			{StringRenaming, []};
		false -> 
			{FillStr, []}
	end;
create_output({'\\',_,{closure,Events},SPAN}, Slice, FillStr) ->
	case lists:member(SPAN, Slice, FillStr) of 
		true -> 
			{"\\\\ {|" ++ printer:string_list(Events) ++ "|}", []};
		false -> 
			{FillStr, []}
	end;
create_output({';',_,SPAN}, Slice, FillStr) ->
	case lists:member(SPAN, Slice, FillStr) of 
		true -> 
			{";", []};
		false -> 
			{FillStr, []}
	end.


extract_spans(Slice, Digraph) ->
	[ begin {V,{_,SPAN}} = digraph:vertex(Digraph, V), SPAN end 
		|| V <- digraph:vertices(Digraph), lists:member(V,Slice)].

ask_code(Process, Args) ->
	csp_process:send_message2regprocess(codeserver,{ask_code,Process,Args,self()}),
	receive 
		{code_reply,PBody} -> 
			PBody
	end.
