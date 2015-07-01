-module(csp_slicer).

-export([get_total_slices/1, get_slices/2]).

-include("csp_tracker.hrl").

get_total_slices(G) ->
	length(get_all_slices(G)).

get_all_slices(G) ->
	All = 
		[ case digraph:vertex(G, V) of
				{V, {?SLICE, _}} ->
					Es0 = digraph:in_edges(G, V),
					InfoEs0 = [ digraph:edge(G, E) || E <- Es0 ],
					Os0 = digraph:out_edges(G, V),
					InfoOs0 = [ digraph:edge(G, E) || E <- Os0 ],
					Sync0 = 
						[V1 || {_,V1,_,"sync"} <- InfoEs0] 
						++ [V2 ||{_,_,V2,"sync"} <- InfoOs0],
					{lists:min([V|Sync0]), lists:usort([get_actual_vertex(G, VS) || VS <- [V|Sync0]])};
				_ -> 
					[]
		  end || V <- digraph:vertices(G) ],
	Result = lists:usort(lists:flatten(All)),
	% print_from_digraph(G, "track_temp", []),
	% io:format("Slice: ~w\n",[Result]),
	Result.


get_actual_vertex(G, V) ->
	Es = digraph:in_edges(G, V),
	InfoEs = [ digraph:edge(G, E) || E <- Es ],
	case [V1 || {_,V1,_,"control"} <- InfoEs] of 
		[] ->
			[];
		[Next|_] -> 
			case digraph:vertex(G, Next) of 
				{_,{"->",_}} ->
					Es2 = digraph:in_edges(G, Next),
					InfoEs2 = [ digraph:edge(G, E) || E <- Es2 ],
					case [V1||{_,V1,_,"control"} <- InfoEs2] of 
						[] ->
							[];
						[Next2|_] -> 
							Es3 = digraph:in_edges(G, Next2),
							InfoEs3 = [ digraph:edge(G, E) || E <- Es3 ],
							Os = digraph:out_edges(G, Next2),
							InfoOs = [ digraph:edge(G, E) || E <- Os ],
							Sync = 
								[ V1 ||{_,V1,_,"sync"} <- InfoEs3]
								++ [ V2 ||{_,_,V2,"sync"} <- InfoOs],
							[lists:min([Next2 | Sync])]
					end;
				_ ->
					[Next]
			end
	end.

get_slices(G, Selected) ->
	OrderedSlices = lists:sort(get_all_slices(G)),
	{_, SelectedVertex}  = lists:nth(Selected, OrderedSlices),
	% io:format("All: ~w\n", [OrderedSlices]),
	StartSet = lists:flatten(SelectedVertex),
	lists:usort(calculate_slice(G, StartSet,[])).

calculate_slice(G, [From | Tail], Slice) ->
	{NList, NSlice} = 
		case lists:member(From, Slice) of  
			true -> 
				{Tail, Slice};
			false -> 
				Es = digraph:in_edges(G, From),
				InfoEs = [ digraph:edge(G, E) || E <- Es ],
				Os = digraph:out_edges(G, From),
				InfoOs = [ digraph:edge(G, E) || E <- Os ],
				NList0 =
					case [V1||{_,V1,_,"control"} <- InfoEs] of 
						[] ->
							Tail;
						[Next|_] -> 
							[Next | Tail]
					end,
				FromSync = 
					[ V1 ||{_,V1,_,"sync"} <- InfoEs] 
					++ [ V2 ||{_,_,V2,"sync"} <- InfoOs],
				{NList0 ++ FromSync, [From |Slice]}
		end,
	calculate_slice(G, NList, NSlice);
calculate_slice(_G, [], Slice) ->
	Slice.


% print_from_digraph(Digraph, NameFile, Slice) ->
% 	NodesSlice = 
% 		lists:flatten([
% 			begin 
% 				{Id, {Label, SPAN}} = digraph:vertex(Digraph, VD),
% 				printer:string_vertex_dot(Id, Label, SPAN, Slice)
% 			end || VD <- digraph:vertices(Digraph)]),
% 	EdgesSlice = 
% 		lists:flatten([
% 			begin 
% 				{_, V1, V2, Type} = digraph:edge(Digraph, ED),
% 				printer:string_edge_dot(V1, V2, Type)
% 			end || ED <- digraph:edges(Digraph)]),
% 	file:write_file(NameFile ++ ".dot", 
% 		list_to_binary("digraph " ++ NameFile ++ " {" ++ NodesSlice ++ EdgesSlice ++ "\n}")),
% 	os:cmd("dot -Tpdf " ++ NameFile ++ ".dot > " ++ NameFile ++ ".pdf").

% get_slices(G) ->
% 	Slice = calculate_slice(G, leaves(G), [], []),
% 	Slice.


% leaves(G) ->
% 	[ V || 
% 		V <- digraph:vertices(G), 
% 		length(digraph_utils:reachable([V], G)) == 1 ].

% calculate_slice(G, [V | Tail], Slice, Acc) ->
% 	% io:format("V: ~p\n",[V]),
% 	{NList, NSlice, NAcc} = 
% 		case lists:member(V, Slice) of 
% 			true -> 
% 				{Tail, Slice, []};
% 			false -> 
% 				case digraph:vertex(G, V) of
% 					{V, {?SLICE_END,_}} ->
% 						{Lasts, SliceV} = store_slice(G, [V], {[],[]}),
% 						{Lasts ++ Tail, SliceV ++ Slice, []};
% 					{V, {?SLICE_START,_}} -> 
% 						{Lasts, SliceV} = store_slice(G, [V], {[],[]}),
% 						{Tail, [V | Acc] ++ Slice, []};
% 					_ -> 
% 						Es = digraph:in_edges(G, V),
% 						InfoEs = [ digraph:edge(G, E) || E <- Es ],
% 						NList0 =
% 							case [V1 || {_,V1,_,"control"} <- InfoEs] of 
% 								[] ->
% 									Tail;
% 								[Next|_] -> 
% 									[Next | Tail]
% 							end,
% 						{NList0, Slice, [V | Acc]}
% 				end
% 		end,
% 	% io:format("New from V: ~p\n",[{NList, NSlice}]),
% 	calculate_slice(G, NList, NSlice, NAcc);
% calculate_slice(_, [], Slice, _) ->
% 	Slice.

% store_slice(G, [Current | Tail], {Lasts, Slice}) ->
% 	case lists:member(Current, Slice) of  
% 		true -> 
% 			store_slice(G, Tail, {Lasts, Slice});
% 		false -> 
% 			Es = digraph:in_edges(G, Current),
% 			InfoEs = [ digraph:edge(G, E) || E <- Es ],
% 			Os = digraph:out_edges(G, Current),
% 			InfoOs = [ digraph:edge(G, E) || E <- Os ],
% 			{NList, NLasts} =
% 				case [V1 || {_,V1,_,"control"} <- InfoEs] of 
% 					[] ->
% 						{Tail, Lasts};
% 					[Next|_] -> 
% 						case digraph:vertex(G, Current) of 
% 							{Current, {?SLICE_START,_}} ->
% 								{Tail, [Next|Lasts]};
% 							_ ->
% 								{[Next | Tail], Lasts}
% 						end
% 				end,
% 			FromSync = 
% 				[ V1 || {_,V1,_,"sync"} <- InfoEs] 
% 				++ [ V2 || {_,_,V2,"sync"} <- InfoOs],
% 			store_slice(G, NList ++ FromSync, {NLasts, [Current | Slice]})
% 	end;
% store_slice(_, [], LastsSlice) ->
% 	LastsSlice.

% %  csp_tracker:track('ex1.csp', 'MAIN',[10]). 

% % TODOS:
% % - Aclarar dudas de sincronizacion
% % - Tratar los nodos sincronizados para slice_start sin slice_end 


