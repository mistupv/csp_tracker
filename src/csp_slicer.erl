-module(csp_slicer).

-export([get_total_slices/1, get_slices/2]).

-define(SLICE, "slice").
-define(SLICE_START, "slice_start").
-define(SLICE_END, "slice_end").

get_total_slices(G) ->
	length(get_all_slices(G)).

get_all_slices(G) ->
	All = 
		[ case digraph:vertex(G, V) of
				{V, {?SLICE, _}} ->
					[V];
				_ -> 
					[]
		  end || V <- digraph:vertices(G) ],
	lists:flatten(All).

get_slices(G, Selected) ->
	OrderedSlices = lists:sort(get_all_slices(G)),
	SelectedVertex = lists:nth(Selected, OrderedSlices),
	lists:usort(calculate_slice(G, [SelectedVertex],[])).

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


