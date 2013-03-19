-module(printer).

-export([loop/1,nonid_loop/0,string_arguments/1]).


loop(Option) ->
	register(nonid, spawn(printer,nonid_loop,[])),
	case Option of
	     only_externals -> loop(0,false);
	     _ -> loop(0,true)
	end.

loop(Free,PrintInternals) ->
	receive
		{print,Event,Pid} -> 
		        case PrintInternals of
		             true ->
		               	print_event(Event);
		             false ->
		             	case atom_to_list(Event) of
		             	     [$ ,$ ,$ ,$t,$a,$u|_] -> ok;
		             	     [$ ,$ ,$ ,$t,$i,$c,$k|_] -> ok;
		             	     _ -> print_event(Event)
		             	end
		        end,
			Pid!{printed,Event},
			loop(Free,PrintInternals);
%		{no_print,Event,Pid} -> 
%			Pid!{no_printed,Event,io_lib:format("~p\n",[Event])},
%			loop(Free);
		{print_sync,NodeA,NodeB,Pid} ->
		     Pid!{printed_sync,NodeA,NodeB},
		     add_to_file(integer_to_list(NodeA)++" -> "++integer_to_list(NodeB)
	                  ++"[style=dotted, color=red, arrowhead=none,constraint=false];\n"),
		     loop(Free,PrintInternals);
		{create_graph,Process,Parent,Pid} ->
			{Graph,NFree,NParent} = create_graph_string(Process,Free,Parent),
			case Graph of
			     "" -> 
			     	Pid ! {created,Parent};
			     _ ->
			     	Pid ! {created,NParent},
				add_to_file(Graph)
			end,
			loop(NFree,PrintInternals);
		{create_graph_no_ided,Process,Parent,Pid} ->
		        %io:format("~p\n~p\n",[Process,Parent]),
			{Graph,IdAno} = create_graph_string_no_ided(Process,Parent),
			%io:format("{Parent,Free,NFree}: ~p\n",[{Parent,Free,NFree}]),
			add_to_file("//-> "++atom_to_list(IdAno)++"\n"
			            ++Graph++
			            "//<- "++atom_to_list(IdAno)++"\n"),
			Pid ! {created_no_id,IdAno},
			loop(Free,PrintInternals);
		{remove_graph_no_ided,IdAno,Pid} ->
			remove_from_file("//-> "++atom_to_list(IdAno),
			                 "//<- "++atom_to_list(IdAno)),
			Pid ! {removed,IdAno},
			loop(Free,PrintInternals);
%		{print_graph_temporal,Graph,IdAno,Pid} ->
%			add_to_file("//-> "++atom_to_list(IdAno)
%			            ++Graph++
%			            "//<- "++atom_to_list(IdAno)),
%			Pid ! {printed_graph,Graph,IdAno},
%			loop(Free,PrintInternals);
		stop -> 
			nonid!stop,
			ok
	end.
	
	
print_event(Event) ->
	case csp_parsing:fake_process_name(atom_to_list(Event)) of
	     true -> ok;
	     false -> io:format("~s\n",[atom_to_list(Event)])
	end.
	
add_to_file(String) ->
	{ok, IODevice} = file:open("track.dot",[read]),
	Read = read_file(IODevice),
	file:close(IODevice),
	file:write_file("track.dot", list_to_binary(Read ++String++"}")),
	os:cmd("dot -Tpdf track.dot > track.pdf").
	
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
	SEdge = 
	  case {NParent =/= Parent, SProcess, Parent} of
	       {_,_,-1} -> "";
	       {_,"",_} -> "";
	       {false,_,_} -> "";
	       {true,_,_} -> string_edge(Parent,Free)
	  end,
	{SEdge ++ SProcess,NFree,NParent}.
	
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
	{string_vertex(Free,atom_to_list(Event)++string_channels(Channels),SPANevent)++
	 string_vertex(Free+1,"->",SPANarrow)++
	 string_edge(Free,Free+1),
	 Free+2,Free+1};
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
create_graph({procRenaming,{rename,Original,Renamed},_,SPAN},Free) ->
	StringRenaming = "[["++ atom_to_list(Original) ++ 
	                 " <- " ++ atom_to_list(Renamed) ++ "]]",
	{string_vertex(Free,StringRenaming,SPAN),Free+1,Free};
create_graph({'\\',_,{closure,Events},SPAN},Free) ->
	{string_vertex(Free,"\\\\ {|"++string_list(Events)++"|}",SPAN),
	 Free+1,Free};
create_graph({';',NodesFinished,SPAN},Free) ->
	{lists:append([string_edge(Node,Free) || Node <- NodesFinished]) ++ 
	 string_vertex(Free,";",SPAN),Free+1,Free}.

string_vertex(Id,Label,SPAN) ->
	{FL,FC,TL,TC} = extractFromTo(SPAN,Label),
	%string_vertex(Id,Label).
	integer_to_list(Id)++" "++"[shape=ellipse, label=\""
	++integer_to_list(Id)++" .- " ++ Label ++
	"\\nfrom ("++ integer_to_list(FL) ++ "," ++ integer_to_list(FC) ++
	") to (" ++ integer_to_list(TL) ++ "," ++ integer_to_list(TC) ++")\\l\"];\n".
	
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
	
string_edge(From,To) ->
	integer_to_list(From)++" -> "++integer_to_list(To)
	++" [color=black, penwidth=3];\n".
	

	
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
