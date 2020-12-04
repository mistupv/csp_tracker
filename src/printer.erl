-module(printer).

-export([
	%% 'printer' process API
	start_and_register_once/0,
	start_and_register_once/2,
	start_and_register_until_success/0,
	get_info_graph/0,
	get_info_graph_no_stop/0,
	get_trace/0,
	get_steps/0,
	print/1,
	print_sync/2,
	create_graph/2,
	create_graph_no_ided/2,
	remove_graph_no_ided/1,
	%% spawn points
	loop/2, nonid_loop/0, digraph_loop/0,
	%% current process API
	string_arguments/1, add_to_file/2,
	string_vertex_dot/4, string_edge_dot/3,
	string_channels/1, string_list/1,
	extractFromTo/2
]).

-include("csp_tracker.hrl").
-include("csp_bench_size.hrl").

%% PUBLIC API (process control) %%
start_and_register_once() ->
	start_and_register_once(all, false).
start_and_register_once(Option, LiveSaving) ->
	csp_util:register_once(printer, fun () -> loop(Option, LiveSaving) end).

start_and_register_until_success() ->
	start_and_register_until_success(all, false).
start_and_register_until_success(Option, LiveSaving) ->
	case start_and_register_once(Option, LiveSaving) of
		true -> start_and_register_until_success(Option, LiveSaving);
		badarg -> start_and_register_until_success(Option, LiveSaving);
		ok -> ok
	end.

get_info_graph() ->
	get_info_graph(info_graph).

get_info_graph_no_stop() ->
	get_info_graph(info_graph_no_stop).

get_info_graph(Type) ->
	csp_util:send_message(printer, {Type, self()}),
	receive
		{info_graph, InfoGraph} -> InfoGraph
	after 1000 -> throw(timeout)
	end.

get_trace() ->
	csp_util:send_message(printer, {get_trace, self()}),
	receive
		{trace, Trace} -> Trace
	end.

get_steps() ->
	csp_util:send_message(printer, {get_steps, self()}),
	receive
		{steps, Steps} -> Steps
	end.

print(Message) ->
	csp_util:send_message(printer, store_step),
	?PRINTER_SEND_LOG_MEMORY,
	case csp_util:tracker_mode() of
		track ->
			csp_util:send_message(printer, {print, Message, self()}),
			receive
				{printed, Message} -> ok
			end;
		run -> ok
	end.

print_sync(NodeA, NodeB) ->
	case csp_util:tracker_mode() of
		track ->
			csp_util:send_message(printer, {print_sync, NodeA, NodeB, self()}),
			receive
				{printed_sync, NodeA, NodeB} -> ok
			end;
		run ->
			csp_util:send_message(printer, timestamp_update),
			ok
	end.

remove_graph_no_ided(IdAno) ->
	case csp_util:tracker_mode() of
		track ->
			csp_util:send_message(printer, {remove_graph_no_ided, IdAno, self()}),
			receive
				{removed, IdAno} -> ok
			end;
		run -> ok
	end.

create_graph(Process, GraphParent) ->
	case csp_util:tracker_mode() of
		track ->
			csp_util:send_message(printer, {create_graph, Process, GraphParent, self()}),
			receive
				{created, NGraphParent} -> NGraphParent
			end;
		run ->
			csp_util:send_message(printer, timestamp_update),
			GraphParent
	end.

create_graph_no_ided(Prefixing, GraphParent) ->
	case csp_util:tracker_mode() of
		track ->
			csp_util:send_message(printer, {create_graph_no_ided, Prefixing, GraphParent, self()}),
			receive
				{created_no_id, IdAno} -> IdAno
			end;
		run -> 0
	end.

%% PROCESS API %%

loop(Option,LiveSaving) ->
	csp_util:register_once(nonid, fun() -> printer:nonid_loop() end),
	csp_util:register_once(digraph, fun() -> printer:digraph_loop() end),
	PrintInternals =
		case Option of
			only_externals -> false;
			all -> true
		end,
	loop(0,PrintInternals,LiveSaving,{{0,0,0,0},"",""},0).

%The functionality for showing non-executed code has been diasabled.
loop(Free,PrintInternals,LiveSaving,State,Steps) ->
	receive
		timestamp_update ->
			{{N,E,S,_},G,Trace} = State,
			loop(Free,PrintInternals,LiveSaving,{{N,E,S,erlang:monotonic_time()},G,Trace},Steps);
		store_step ->
			NSteps =
				case LiveSaving of
					true -> Steps;
					false -> Steps + 1
				end,
			loop(Free,PrintInternals,LiveSaving,State,NSteps);
		printout_memory ->
			?LOG_MEMORY(Steps),
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		{print,Event,Pid} ->
			EventTrace =
				case PrintInternals of
					true ->
						print_event(Event);
					false ->
						case atom_to_list(Event) of
							"   tau" ++ _ -> "";
							"   tick" ++ _ -> "";
							_ -> print_event(Event)
						end
				end,
			{InfoGraph,G,Trace} = State,
			NTrace =
				case LiveSaving of
					true ->
						io:format("~s",[EventTrace]),
						Trace;
					false ->
						Trace ++ EventTrace
				end,
			Pid!{printed,Event},
			loop(Free,PrintInternals,LiveSaving,{InfoGraph,G,NTrace},Steps);
		{print_sync,NodeA,NodeB,Pid} ->
			Pid!{printed_sync,NodeA,NodeB},
			{{N,E,S,_},G,Trace} = State,
			digraph_add_edge(NodeA, NodeB, "sync"),
			StringSyncEdge = string_edge_dot(NodeA, NodeB, "sync"),
			NG =
				case LiveSaving of
					true ->
						add_to_file(StringSyncEdge,false),
						G;
					false ->
						G ++ StringSyncEdge
				end,
			loop(Free,PrintInternals,LiveSaving,{{N,E,S + 1,erlang:monotonic_time()},NG,Trace},Steps);
		{create_graph,Process,Parent,Pid} ->
			{Graph,NFree,NParent,TotalCreated} = create_graph_string(Process,Free,Parent),
			{{N,E,S,_},G,Trace} = State,
			NG = 
				case Graph of
					"" ->
						Pid ! {created,Parent},
						G;
					_ ->
						Pid ! {created,NParent},
						case LiveSaving of
							true ->
								add_to_file(Graph,false),
								G;
							false ->
								G ++ Graph
						end
				end,
			{NN,NE} = 
				case TotalCreated of 
					0 -> {N,E};
					1 -> {N + 1,E};
					2 -> {N + 1,E + 1};
					3 -> {N + 2,E + 1}
				end,
			loop(NFree,PrintInternals,LiveSaving,{{NN,NE,S,erlang:monotonic_time()},NG,Trace},Steps);
		{create_graph_no_ided,Process,Parent,Pid} ->
			{_Graph,IdAno} = create_graph_string_no_ided(Process,Parent),
			Pid ! {created_no_id,IdAno},
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		{remove_graph_no_ided,IdAno,Pid} ->
			Pid ! {removed,IdAno},
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		{info_graph,Pid} ->
			G = digraph_get(),
			Pid ! {info_graph, {State, G}},
			% Because it is only called when finished
			finish_computation();
		{info_graph_no_stop,Pid} ->
			G = digraph_get(),
			Pid ! {info_graph, {State, G}},
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		{get_trace, Pid} ->
			{_,_,Trace} = State,
			Pid ! {trace, Trace},
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		{get_steps, Pid} ->
			Pid ! {steps, Steps},
			loop(Free,PrintInternals,LiveSaving,State,Steps);
		stop -> 
			finish_computation();
		{stop, Pid} -> 
			finish_computation(),
			Pid ! stopped
	end.
	
finish_computation() ->
	catch csp_util:stop_and_wait(nonid),
	catch csp_util:stop_and_wait(digraph),
	ok.

print_event(Event) ->
	case {csp_parsing:fake_process_name(atom_to_list(Event)),atom_to_list(Event) == ?SLICE} of
		{false, false} -> io_lib:format("~s\n",[atom_to_list(Event)]);
		_ -> ""
	end.
	
add_to_file(String,NoOutput) ->
	{ok, IODevice} = file:open("track.dot",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	file:write_file("track.dot", list_to_binary(Read ++String++"}")),
	case NoOutput of 
		false -> os:cmd("dot -Tpdf track.dot > track.pdf");
		true -> ok
	end.
	
read_file(IODevice) ->
	read_file(IODevice,[],[]).

read_file(IODevice,PrevData,Acc) ->
	case io:request(IODevice, {get_line, ''}) of
		eof -> Acc;
		Data -> read_file(IODevice,Data,Acc++PrevData)
	end.	
	
create_graph_string(Process,Free,Parent) ->
	{SProcess,NFree,NParent} = create_graph_internal(Process,Free),
	{SEdge,TotalNodesEdges} = 
	  case {NParent =/= Parent, SProcess, Parent} of
			{_,"",_} -> {"",0};
			{_,_,-1} -> {"",1};
			{false,_,_} -> {"",1};
			{true,_,_} -> {string_edge(Parent,Free),2}
	  end,
	NTotalNodesEdges = 
		case Process of 
			{prefix,_,_,_,_,_} ->
				TotalNodesEdges + 1;
			{renamed_event,_,{prefix,_,_,_,_,_}} ->
				TotalNodesEdges + 1;
			_ ->
				TotalNodesEdges
		end,
	{SEdge ++ SProcess,NFree,NParent,NTotalNodesEdges}.
	
create_graph_string_no_ided({prefix,SPANevent,Channels,Event,_,SPANarrow},Parent) ->
	IdA = nonid_get(),
	IdB = nonid_get(),
	SProcess =
	 string_vertex_no_ided(IdA,atom_to_list(Event)++string_channels(Channels),SPANevent)++
	 string_vertex_no_ided(IdB,"->",SPANarrow)++
	 atom_to_list(IdA)++" -> "++atom_to_list(IdB)
	 ++" [color=black, penwidth=3];\n",
	SEdge = 
	  case Parent of
			-1 -> "";
			_ -> integer_to_list(Parent) ++ " -> " ++ atom_to_list(IdA)
				++ " [color=black, penwidth=3];\n"
	  end,
	{SEdge ++ SProcess,IdA}.

create_graph_internal({prefix,SPANevent,Channels,Event,_,SPANarrow},Free) ->
	Str = 
		string_vertex(Free, atom_to_list(Event) ++ string_channels(Channels),SPANevent) 
	 	++ string_vertex(Free+1,"->",SPANarrow) ++ string_edge(Free,Free+1),
	{Str, Free+2,Free+1};
create_graph_internal({renamed_event,Executed,{prefix,SPANevent,Channels,Event,_,SPANarrow}},Free) ->
	RenamingInfo = 
		case Executed of 
			Event -> "";
			_ -> " [[" ++ atom_to_list(Event) ++ "<-" ++ atom_to_list(Executed) ++ "]]"
		end,
	Str = 
		string_vertex(Free,atom_to_list(Executed) ++ string_channels(Channels)
		++ RenamingInfo,SPANevent) ++ string_vertex(Free+1,"->",SPANarrow) 
		++ string_edge(Free,Free+1),
	{Str, Free+2,Free+1};
create_graph_internal({'|~|',_,_,SPAN},Free) ->
	{string_vertex(Free,"|~|",SPAN),Free+1,Free};
create_graph_internal({'|~|',_,_,Selected,SPAN},Free) ->
	{string_vertex(Free,"|~|." ++ atom_to_list(Selected),SPAN),Free+1,Free};
create_graph_internal({'[]',_,_,SPAN},Free) ->
	{string_vertex(Free,"[]",SPAN),Free+1,Free};
create_graph_internal({'ifte',Condition,_,_,SPAN1,_,_},Free) ->
	{string_vertex(Free,"<<"++atom_to_list(Condition)++">>",SPAN1),Free+1,Free};
create_graph_internal({agent_call,SPAN,ProcessName,Arguments},Free) ->
	case csp_parsing:fake_process_name(atom_to_list(ProcessName)) of
		true -> {"",Free,Free};
		false ->
			{string_vertex(Free,atom_to_list(ProcessName)++string_arguments(Arguments),SPAN),
				Free+1,Free}
	end;
create_graph_internal({sharing,{closure,Events},_,_,SPAN},Free) ->
	{string_vertex(Free,"[|{|"++string_list(Events)++"|}|]",SPAN),Free+1,Free};
create_graph_internal({'|||',_,_,SPAN},Free) ->
	{string_vertex(Free,"|||",SPAN),Free+1,Free};
create_graph_internal({skip,SPAN},Free) ->
	{string_vertex(Free,"SKIP",SPAN),Free+1,Free};
create_graph_internal({stop,SPAN},Free) ->
	{string_vertex(Free,"STOP",SPAN),Free+1,Free};
create_graph_internal({procRenaming,Renamings,_,SPAN},Free) ->
	StringRenamingList0 = lists:flatten(
		[atom_to_list(Original) ++ " <- " ++ atom_to_list(Renamed) ++ ","
			|| {rename,Original,Renamed} <- Renamings]),
	StringRenamingList = lists:reverse(tl(lists:reverse(StringRenamingList0))),
	StringRenaming = "[[" ++ StringRenamingList ++ "]]",
	{string_vertex(Free,StringRenaming,SPAN),Free+1,Free};
create_graph_internal({'\\',_,{closure,Events},SPAN},Free) ->
	{string_vertex(Free,"\\\\ {|"++string_list(Events)++"|}",SPAN),Free+1,Free};
create_graph_internal({';',NodesFinished,SPAN},Free) ->
	{lists:append([string_edge(Node,Free) || Node <- NodesFinished]) ++
		string_vertex(Free,";",SPAN),Free+1,Free}.

string_vertex(Id,Label,SPAN) ->
	digraph_add_vertex(Id, {Label, SPAN}),
	string_vertex_dot(Id,Label,SPAN,[]).
	
string_vertex_no_ided(Id,Label,SPAN) ->
	{FL,FC,TL,TC} = extractFromTo(SPAN,Label),
	atom_to_list(Id) ++ " " ++ "[shape=ellipse, label=\"" ++ Label ++
		"\\nfrom (" ++ integer_to_list(FL) ++ "," ++ integer_to_list(FC) ++
		") to (" ++ integer_to_list(TL) ++ "," ++ integer_to_list(TC) ++")\\l\"];\n".

string_edge(From, To) ->
	digraph_add_edge(From, To, "control"),
	string_edge_dot(From, To, "control").
	
string_vertex_dot(Id,Label,SPAN, Slice) ->
	Style = 
		case lists:member(Id, Slice) of 
			true -> " style=filled color=\"gray\" fontcolor=\"black\" fillcolor=\"gray\"";
			false -> ""
		end,
	{FL,FC,TL,TC} = extractFromTo(SPAN,Label),
	integer_to_list(Id) ++ " " ++ "[shape=none, label=<<table PORT=\"p\"><tr><td>"
		++ integer_to_list(Id) ++ "</td><td>" ++ escape_html(Label)
		++ "</td></tr><tr><td colspan=\"2\">(" ++ integer_to_list(FL) ++ "," ++ integer_to_list(FC)
		++ ") to (" ++ integer_to_list(TL) ++ "," ++ integer_to_list(TC) ++")</td></tr></table>>"
		++ Style ++ "];\n".

string_edge_dot(From, To, "control") ->
	integer_to_list(From) ++ ":p -> " ++ integer_to_list(To) ++ ":p [color=black, penwidth=3];\n";
string_edge_dot(NodeA, NodeB, "sync") ->
	integer_to_list(NodeA) ++ ":p -> " ++ integer_to_list(NodeB)
		++ ":p [style=dashed, penwidth=3, color=red, arrowhead=none, constraint=false];\n".
	
string_list([Event]) -> 
	atom_to_list(Event);
string_list([Event|Events]) -> 
	atom_to_list(Event) ++ "," ++ string_list(Events);
string_list([]) ->
	[].

string_arguments(Pars = [_|_]) ->
	"(" ++ string_arguments_aux(Pars)  ++ ")";
string_arguments([]) -> "".

string_arguments_aux([Par]) when is_atom(Par) ->
	atom_to_list(Par);
string_arguments_aux([Par|Pars]) when is_atom(Par) ->
	atom_to_list(Par) ++ ","++ string_arguments_aux(Pars);
string_arguments_aux([Par]) when is_integer(Par) ->
	integer_to_list(Par);
string_arguments_aux([Par|Pars]) when is_integer(Par) ->
	integer_to_list(Par) ++ ","++ string_arguments_aux(Pars);
string_arguments_aux([]) -> "".
		
extractFromTo({src_span,FL,FC,TL,TC,_,_},_) ->
	{FL,FC,TL,TC};
extractFromTo({src_position,FL,FC,_,_},Op) ->
	{FL,FC,FL,FC+length(Op)}.
	
string_channels([{in,Event}|Tail]) when is_atom(Event) ->
	"?" ++ atom_to_list(Event) ++ string_channels(Tail);
string_channels([{in,Event}|Tail]) when is_list(Event) ->
	"?" ++ Event ++ string_channels(Tail);
string_channels([{'inGuard',Event,_}|Tail]) when is_atom(Event) ->
	"?" ++ atom_to_list(Event) ++ string_channels(Tail);
string_channels([{out,Event}|Tail]) when is_atom(Event) ->
	"!" ++ atom_to_list(Event) ++ string_channels(Tail);
string_channels([{in,Event}|Tail]) when is_integer(Event) ->
	"?" ++ integer_to_list(Event) ++ string_channels(Tail);
string_channels([{'inGuard',Event,_}|Tail]) when is_integer(Event) ->
	"?" ++ integer_to_list(Event) ++ string_channels(Tail);
string_channels([{out,Event}|Tail]) when is_integer(Event) ->
	"!" ++ integer_to_list(Event) ++ string_channels(Tail);
string_channels([{'inGuard',_,List}|Tail]) ->
	"?VAR:{" ++ string_arguments_aux(List) ++ "}" ++ string_channels(Tail);
string_channels([Other|Tail]) ->
	io_lib:format("~p", [Other]) ++ string_channels(Tail);
string_channels([]) -> "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nonid_loop() ->
	nonid_loop(0).
	
nonid_loop(Fresh) ->
	receive
		{get,Pid} -> 
			Pid!{idano,list_to_atom("a"++integer_to_list(Fresh))},
			nonid_loop(Fresh+1);
		stop ->
			ok;
		{stop, Pid} ->
			Pid!stopped,
			ok
	end.

nonid_get() ->
	csp_util:send_message(nonid, {get, self()}),
	receive
		{idano, Id} -> Id
	after 1000 -> throw(timeout)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


digraph_loop() ->
	digraph_loop({[], []}).

digraph_loop(G = {N, E}) ->
	receive 
		{get, Pid} ->
			Pid ! {digraph, G},
			digraph_loop(G);
		{add, vertex, V, Label} ->
			digraph_loop({[{V, Label} | N], E});
		{add, edge, V1, V2, Label} ->
			digraph_loop({N, [{V1, V2, Label} | E]});
		stop ->
			ok;
		{stop, Pid} ->
			Pid!stopped,
			ok
	end.

digraph_get() ->
	csp_util:send_message(digraph, {get, self()}),
	receive
		{digraph, G} -> G
	after 1000 -> throw(timeout)
	end.

digraph_add_vertex(V, Label) ->
	csp_util:send_message(digraph, {add, vertex, V, Label}).

digraph_add_edge(Source, Target, Label) ->
	csp_util:send_message(digraph, {add, edge, Source, Target, Label}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escape_html(Text) ->
	T1 = re:replace(Text, "&", "\\&amp;", [{return,list}]),
	T2 = re:replace(T1, ">", "\\&gt;", [{return,list}]),
	T3 = re:replace(T2, "<", "\\&lt;", [{return,list}]),
	re:replace(T3, "\\\\", "\\&#92;", [{return,list}]).
