-module(printer).

-export([loop/2, nonid_loop/0, digraph_loop/0, 
		string_arguments/1, add_to_file/2,
		string_vertex_dot/4, string_edge_dot/3,
		string_channels/1, string_list/1,
		extractFromTo/2]).

-include("csp_tracker.hrl").

loop(Option,LiveSaving) ->
	case lists:member(nonid,registered()) of
	     true -> 
	     	ok;
	     false -> 
	     	register(nonid, spawn(printer,nonid_loop,[]))
	end,
	case lists:member(digraph,registered()) of
	     true -> 
	     	ok;
	     false -> 
	     	register(digraph, spawn(printer,digraph_loop,[]))
	end,
	case Option of
	     only_externals -> loop(0,false,LiveSaving,{{0,0,0,0},"",""});
	     all -> loop(0,true,LiveSaving,{{0,0,0,0},"",""})
	end.

%The functionality for showing non-executed code has been diasabled. In order to activate it again uncomment lines add_to_file and remove_from_file from the no_ided messages services. 
loop(Free,PrintInternals,LiveSaving,State) ->
	receive
		{print,Event,Pid} -> 
				EventTrace = 
			        case PrintInternals of
			             true ->
			               	print_event(Event);
			             false ->
			             	case atom_to_list(Event) of
			             	     [$ ,$ ,$ ,$t,$a,$u|_] -> "";
			             	     [$ ,$ ,$ ,$t,$i,$c,$k|_] -> "";
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
			loop(Free,PrintInternals,LiveSaving,{InfoGraph,G,NTrace});
%		{no_print,Event,Pid} -> 
%			Pid!{no_printed,Event,io_lib:format("~p\n",[Event])},
%			loop(Free);
		{print_sync,NodeA,NodeB,Pid} ->
		     Pid!{printed_sync,NodeA,NodeB},
		     		     {{N,E,S,_},G,Trace} = State,
		     digraph!{add, edge, NodeA, NodeB, "sync"},
		     StringSyncEdge = 
		  		string_edge_dot(NodeA, NodeB, "sync"),
		     NG = 
			     case LiveSaving of 
			     	true ->
			     		add_to_file(StringSyncEdge,false),
			     		G;
				    false ->
				    	G ++ StringSyncEdge
				 end,
		     loop(Free,PrintInternals,LiveSaving,{{N,E,S + 1,now()},NG,Trace});
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
			loop(NFree,PrintInternals,LiveSaving,{{NN,NE,S,now()},NG,Trace});
		{create_graph_no_ided,Process,Parent,Pid} ->
		    %io:format("~p\n~p\n",[Process,Parent]),
			{Graph,IdAno} = create_graph_string_no_ided(Process,Parent),
			%io:format("{Parent,Free,NFree}: ~p\n",[{Parent,Free,NFree}]),
			% add_to_file("//-> "++atom_to_list(IdAno)++"\n"
			%             ++Graph++
			%             "//<- "++atom_to_list(IdAno)++"\n"),
			Pid ! {created_no_id,IdAno},
			loop(Free,PrintInternals,LiveSaving,State);
		{remove_graph_no_ided,IdAno,Pid} ->
			% remove_from_file("//-> "++atom_to_list(IdAno),
			%                  "//<- "++atom_to_list(IdAno)),
			Pid ! {removed,IdAno},
			loop(Free,PrintInternals,LiveSaving,State);
%		{print_graph_temporal,Graph,IdAno,Pid} ->
%			add_to_file("//-> "++atom_to_list(IdAno)
%			            ++Graph++
%			            "//<- "++atom_to_list(IdAno)),
%			Pid ! {printed_graph,Graph,IdAno},
%			loop(Free,PrintInternals);
		{info_graph,Pid} ->
			% Because it is only called when finished 
			digraph!{get, self()},
			receive
				{digraph,G} ->
					Pid!{info_graph, {State, G}},
					finish_computation()
			end;
		{info_graph_no_stop,Pid} ->
			% Because it is only called when finished 
			digraph!{get, self()},
			receive
				{digraph,G} ->
					Pid!{info_graph, {State, G}},
					loop(Free,PrintInternals,LiveSaving,State);
			end;
		{get_trace, Pid} ->
			{_,_,Trace} = State,
			Pid!{trace, Trace},
			loop(Free,PrintInternals,LiveSaving,State);
		stop -> 
			finish_computation()
	end.
	
finish_computation() ->
	nonid!stop,
	digraph!stop,
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
		false ->
			os:cmd("dot -Tpdf track.dot > track.pdf");
		true ->
			ok
	end.

	
remove_from_file(String1,String2) ->
	{ok, IODevice} = file:open("track.dot",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	file:write_file("track.dot", 
	                list_to_binary(remove_from_graph(Read,String1,String2,"")++"}")),
	os:cmd("dot -Tpdf track.dot > track.pdf").
	
	
remove_from_graph([Char|Tail],String1,String2,Acc) ->
	case Char of
	     10 -> 
	     	case Acc of
	     	     String1 -> "\n"++remove_from_graph(Tail,String2,"");
	     	     _ -> Acc ++ "\n" ++ remove_from_graph(Tail,String1,String2,"")
	     	end;
	     _ -> 
	     	remove_from_graph(Tail,String1,String2,Acc++[Char])
	end.
	
remove_from_graph([Char|Tail],String,Acc) ->
	case Char of
	     10 -> 
	     	case Acc of
	     	     String -> Tail;
	     	     _ -> remove_from_graph(Tail,String,"")
	     	end;
	     _ -> 
	     	remove_from_graph(Tail,String,Acc++[Char])
	end.
	
read_file(IODevice) -> 
	read_file(IODevice,[],[]).
	
read_file(IODevice,PrevData,Acc) ->
	case io:request(IODevice, {get_line, ''}) of
	     eof -> Acc;
	     Data -> read_file(IODevice,Data,Acc++PrevData)
	     
	end.	
	
create_graph_string(Process,Free,Parent) ->
	{SProcess,NFree,NParent} = create_graph(Process,Free),
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
	nonid!{get,self()},
	receive
		{idano,IdA} -> ok
	end,
	nonid!{get,self()},
	receive
		{idano,IdB} -> ok
	end,
	%io:format("IdA: ~p\nIdB: ~p\n",[IdA,IdB]),
	SProcess = 
	 string_vertex_no_ided(IdA,atom_to_list(Event)++string_channels(Channels),SPANevent)++
	 string_vertex_no_ided(IdB,"->",SPANarrow)++
	 atom_to_list(IdA)++" -> "++atom_to_list(IdB)
	 ++" [color=black, penwidth=3];\n",
	SEdge = 
	  case Parent of
	       -1 -> "";
	       _ -> integer_to_list(Parent)++" -> "++atom_to_list(IdA)
	            ++" [color=black, penwidth=3];\n"
	  end,
	{SEdge ++ SProcess,IdA}.


create_graph({prefix,SPANevent,Channels,Event,_,SPANarrow},Free) ->
	Str = 
		string_vertex(Free, atom_to_list(Event) ++ string_channels(Channels),SPANevent) 
	 	++ string_vertex(Free+1,"->",SPANarrow) ++ string_edge(Free,Free+1),
	% case  atom_to_list(Event) of 
	% 	% ?SLICE -> 
	% 	"sdasd" ->
	% 		{" ", Free,Free};
	% 	_ -> 
			{Str, Free+2,Free+1};
	% end;
create_graph({renamed_event,Executed,{prefix,SPANevent,Channels,Event,_,SPANarrow}},Free) ->
	RenamingInfo = 
		case Executed of 
			Event -> 
				"";
			_ ->
				" [[" ++ atom_to_list(Event) ++ "<-" ++ atom_to_list(Executed) ++ "]]"
		end,
	Str = 
		string_vertex(Free,atom_to_list(Executed) ++ string_channels(Channels)
		++ RenamingInfo,SPANevent) ++ string_vertex(Free+1,"->",SPANarrow) 
		++ string_edge(Free,Free+1),
	% case  atom_to_list(Executed) of 
	% 	% ?SLICE -> 
	% 	"sdasd" ->
	% 		{" ", Free,Free};
	% 	_ -> 
			{Str, Free+2,Free+1};
	% end;
create_graph({'|~|',_,_,SPAN},Free) ->
	{string_vertex(Free,"|~|",SPAN),Free+1,Free};
create_graph({'[]',_,_,SPAN},Free) ->
	{string_vertex(Free,"[]",SPAN),Free+1,Free};
create_graph({'ifte',Condition,_,_,SPAN1,_,_},Free) ->
	{string_vertex(Free,"<<"++atom_to_list(Condition)++">>",SPAN1),Free+1,Free};
create_graph({agent_call,SPAN,ProcessName,Arguments},Free) ->
        case csp_parsing:fake_process_name(atom_to_list(ProcessName)) of
             true -> {"",Free,Free};
             false ->
                {string_vertex(Free,atom_to_list(ProcessName)++string_arguments(Arguments),SPAN),
	         Free+1,Free}
        end;
create_graph({sharing,{closure,Events},_,_,SPAN},Free) ->
	{string_vertex(Free,"[|{|"++string_list(Events)++"|}|]",SPAN),
	 Free+1,Free};
create_graph({'|||',_,_,SPAN},Free) ->
	{string_vertex(Free,"|||",SPAN),Free+1,Free};
create_graph({skip,SPAN},Free) ->
	{string_vertex(Free,"SKIP",SPAN),Free+1,Free};
create_graph({stop,SPAN},Free) ->
	{string_vertex(Free,"STOP",SPAN),Free+1,Free};
create_graph({procRenaming,Renamings,_,SPAN},Free) ->
	% StringRenaming = 
	% 	case is_atom(Renamed) of 
	% 		true ->
	% 			"[[" ++ atom_to_list(Original) ++ 
	%                  " <- " ++ atom_to_list(Renamed) ++ "]]";
	%  		false ->
	%  			"[[" ++ [atom_to_list(ItemRenamed) ++ "," | ItemRenamed <- Renamed]
	%  	end,
	% {rename,Original,Renamed}
	StringRenamingList0 =
		lists:flatten(
			[atom_to_list(Original) ++ " <- " ++ atom_to_list(Renamed) ++ "," 
			 || {rename,Original,Renamed} <- Renamings]),
	StringRenamingList = 
		lists:reverse(tl(lists:reverse(StringRenamingList0))),
	StringRenaming = "[[" ++ StringRenamingList ++ "]]",
	{string_vertex(Free,StringRenaming,SPAN),Free+1,Free};
create_graph({'\\',_,{closure,Events},SPAN},Free) ->
	{string_vertex(Free,"\\\\ {|"++string_list(Events)++"|}",SPAN),
	 Free+1,Free};
create_graph({';',NodesFinished,SPAN},Free) ->
	{lists:append([string_edge(Node,Free) || Node <- NodesFinished]) ++ 
	 string_vertex(Free,";",SPAN),Free+1,Free}.


string_vertex(Id,Label,SPAN) ->
	digraph!{add, vertex, Id, {Label, SPAN}},
	string_vertex_dot(Id,Label,SPAN,[]).
	
string_vertex_no_ided(Id,Label,SPAN) ->
	{FL,FC,TL,TC} = extractFromTo(SPAN,Label),
	%string_vertex(Id,Label).
	atom_to_list(Id)++" "++"[shape=ellipse, label=\""
	++ Label ++
	"\\nfrom ("++ integer_to_list(FL) ++ "," ++ integer_to_list(FC) ++
	") to (" ++ integer_to_list(TL) ++ "," ++ integer_to_list(TC) ++")\\l\"];\n".

%string_vertex(Id,Label) ->
%	integer_to_list(Id)++" "++"[shape=ellipse, label=\""
%	++integer_to_list(Id)++" .- " ++ Label ++ "\"];\n".
	
string_edge(From, To) ->
	digraph!{add, edge, From, To, "control"},
	string_edge_dot(From, To, "control").
	
string_vertex_dot(Id,Label,SPAN, Slice) ->
	Style = 
		case lists:member(Id, Slice) of 
			true ->  
				" style=filled color=\"gray\" fontcolor=\"black\" fillcolor=\"gray\"";
			false -> 
				""
		end,
	{FL,FC,TL,TC} = extractFromTo(SPAN,Label),
	%string_vertex(Id,Label).
	integer_to_list(Id) ++ " " ++ "[shape=ellipse, label=\""
	++ integer_to_list(Id) ++ " .- " ++ Label 
	++ "\\nfrom (" ++ integer_to_list(FL) ++ "," ++ integer_to_list(FC) 
	++ ") to (" ++ integer_to_list(TL) ++ "," ++ integer_to_list(TC) ++")\\l\""
	++ Style ++ "];\n".

string_edge_dot(From, To, "control") ->
	integer_to_list(From) ++ " -> " ++ integer_to_list(To)
	++ " [color=black, penwidth=3];\n";
string_edge_dot(NodeA, NodeB, "sync") ->
	integer_to_list(NodeA) ++ " -> " ++ integer_to_list(NodeB)
	++ "[style=dotted, color=red, arrowhead=none,constraint=false];\n".
	
string_list([Event]) -> 
	atom_to_list(Event);
string_list([Event|Events]) -> 
	atom_to_list(Event)++","++string_list(Events);
string_list([]) ->
	[].

string_arguments(Pars = [_|_]) ->
%	Rest = 
%		case string_arguments_aux(Pars) of
%		     [] -> "";
%		     Rest_ -> "," ++ Rest_
%		end,
%	"(" ++ atom_to_list(Par) ++ Rest  ++")";
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
	"?"++atom_to_list(Event)++string_channels(Tail);
string_channels([{in,Event}|Tail]) when is_list(Event) ->
	"?"++Event++string_channels(Tail);
string_channels([{'inGuard',Event,_}|Tail]) when is_atom(Event) ->
	"?"++atom_to_list(Event)++string_channels(Tail);
string_channels([{out,Event}|Tail]) when is_atom(Event) ->
	"!"++atom_to_list(Event)++string_channels(Tail);
string_channels([{in,Event}|Tail]) when is_integer(Event) ->
	"?"++integer_to_list(Event)++string_channels(Tail);
string_channels([{'inGuard',Event,_}|Tail]) when is_integer(Event) ->
	"?"++integer_to_list(Event)++string_channels(Tail);
string_channels([{out,Event}|Tail]) when is_integer(Event) ->
	"!"++integer_to_list(Event)++string_channels(Tail);
string_channels([{'inGuard',_,List}|Tail]) ->
	"?VAR:{"++string_arguments_aux(List)++"}"++string_channels(Tail);
string_channels([Other|Tail]) ->
	io_lib:format("~p", [Other]) ++string_channels(Tail);
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
			ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


digraph_loop() ->
	digraph_loop({[], []}).

digraph_loop(G = {N, E}) ->
	receive 
		{get, Pid} ->
			Pid!{digraph,G},
			digraph_loop(G);
		{add, vertex, V, Label} ->
			digraph_loop({[{V, Label} | N], E});
		{add, edge, V1, V2, Label} ->
			digraph_loop({N, [{V1, V2, Label} | E]});
		stop ->
			ok
	end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

