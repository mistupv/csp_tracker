-module(csp_slicer_output).

-export([create_slicer_output/4]).

-include("csp_tracker.hrl").


create_slicer_output(Slice, FirstProcess, G, Lines) ->
	SPANS = lists:usort(extract_spans(Slice, G)),
	StrGaps = create_out_process([{atom_to_list(FirstProcess), 0}], G, SPANS, Lines, ?EMPTY, []),
	StrStops = create_out_process([{atom_to_list(FirstProcess), 0}], G, SPANS, Lines, ?NO_DEFINED, []),
	{StrGaps, StrStops}.



create_out_process([{LhsStr0, Node} | Tail], G, Slice, Lines, Filler, Done) -> 
	case digraph:out_neighbours(G, Node) of
		[Next] -> 
			{_,{_,SPAN}} = digraph:vertex(G,Next),
			case lists:member(SPAN, Done) of 
				false -> 
					SameSPAN = search_nodes_same_process(SPAN, G),
					% io:format("~p\n~p\n", [Next, SameSPAN]),
					N =
						case SPAN of 
							{src_span,N0,_,_,_,_,_} ->
								N0;
							{src_position,N0,_,_,_} ->
								N0 
						end,
					LhsStr = 
						case find_lhs_from(Lines, N) of 
							"" -> 
								LhsStr0;
							Other -> 
								Other 
						end,
					% io:format("SPAN: ~p\nN: ~p\nLhsStr: ~p\n", [SPAN, N, LhsStr]), 
					{StrBody, Pending} = create_out_for_nodes(SameSPAN, Slice, Lines, G, Filler),
					% io:format("Pending: ~p\n", [Pending]),
					PendStr = create_out_process(Tail ++ Pending, G,  Slice, Lines, Filler, [SPAN|Done]),
					LhsStr ++ " = " ++ StrBody ++ "\n\n" ++ PendStr;
				true ->
					create_out_process(Tail, G, Slice, Lines, Filler, Done)
			end;
		_ -> 
			""
	end;
create_out_process([], _, _, _, _, _) ->
	"".

create_out_for_nodes([], _, _, _, Filler) ->
	{Filler, []};
create_out_for_nodes(Nodes, Slice, Lines, G, Filler) ->
	{_, {Label, SPAN}} = digraph:vertex(G, hd(Nodes)),
	% io:format("------------\n"),
	% [begin 
	% 		{_, {LabelN, SPANN}} = digraph:vertex(G, N),
	% 		io:format("LabelN: ~s\nSPANN: ~p\n", [LabelN, SPANN])
	% end || N <- Nodes] ,
	% io:format("------------\n"),
	% io:format("-----\n~p\n~p\n-----\n", [SPAN, Label]),
	case lists:member(SPAN, Slice) of 
		true ->  
			Type = 
				case Label of 
					"[]" ->
						bifurcation;
					"|~|" ->
						bifurcation;
					[$<,$<|_] ->
						ifte;
					[$[,$||_] ->
						bifurcation;
					"|||" ->
						bifurcation;
					_ ->
						other
				end,
			case Type of 
				other -> 
					TypeOther = 
						case {lists:member(hd(Label), lists:seq($a,$z)),
							  lists:member(hd(Label), lists:seq($A,$Z))}  of 
							{_,true} -> 
								call;
							{true,_} -> 
								prefix;
							_ -> 
								other
						end,
					% io:format("~s: ~p\n",[Label, TypeOther]),
					NNodes = out_neighbours_control(G, Nodes),
					% io:format("Nodes: ~w\nNNodes: ~w\n", [Nodes, NNodes]),
					Str = read_from_file_content(SPAN, Lines, TypeOther),
					% io:format("Str: ~s\n",[Str]),
					{Rest, Pending} = 
						case TypeOther of 
							call -> 
								{"", [{Label, hd(Nodes)}]};
							_ ->
								create_out_for_nodes(NNodes, Slice, Lines, G, Filler)
						end,
					{Str ++ " " ++ Rest, Pending};
				bifurcation -> 
					AllNodes = out_neighbours_control(G, Nodes),
					{A,B} = separate_spans(AllNodes, G),
					% io:format("A: ~p\nB: ~p\n", [A, B]),
					{StrA, PenA} = create_out_for_nodes(A, Slice, Lines, G, Filler),
					{StrB, PenB} = create_out_for_nodes(B, Slice, Lines, G, Filler),
					% io:format("A: ~p\nB: ~p\nStrA: ~p\nStrB: ~p\n", [A, B, StrA, StrB]),
					{"(" ++ StrA ++  " " ++ Label ++  " " ++ StrB ++ ")", PenA ++ PenB};
				ifte ->
					ThenElse = 
						[begin 
							{_, {LabelN, _}} = digraph:vertex(G, N),
							case LabelN of 
								"<<true>>" ->
									{then,N};
								"<<false>>" ->
									{else,N}
							end
						end || N <- Nodes],
					ThenNodes = lists:flatten([N || {then, N} <- ThenElse]),
					Then = out_neighbours_control(G, ThenNodes),
					ElseNodes = lists:flatten([N || {else, N} <- ThenElse]),
					Else = out_neighbours_control(G, ElseNodes),
					Cond = read_from_file_content(SPAN, Lines, other),
					{StrThen, PenThen} = create_out_for_nodes(Then, Slice, Lines, G, Filler),
					{StrElse, PenElse} = create_out_for_nodes(Else, Slice, Lines, G, Filler),
					% io:format("A: ~p\nB: ~p\nStrA: ~p\nStrB: ~p\n", [A, B, StrA, StrB]),
					{"(" ++ Cond ++ " then " ++ StrThen ++  " else " ++ StrElse ++ ")", PenThen ++ PenElse}
			end;
		false -> 
			case Label of 
				"->" ->
					{Label ++ " " ++ Filler, []};
				_ ->
					{Filler, []}
			end
	end.

out_neighbours_control(G, Nodes) ->
	OsArrow = 
		lists:concat([digraph:out_edges(G, V) || V <- Nodes]),
   lists:flatten(
	   	[begin 
			case digraph:edge(G, O) of 
				{_,_,VArrow,"control"}  ->
					[VArrow];
				_ ->
					[]
			end
		end || O <- OsArrow ]).

separate_spans(All, G) ->
	{_, {_, SPANA}} = digraph:vertex(G, hd(All)),
	AB = 
		[begin 
			{_, {_, SPAN}} = digraph:vertex(G, N),
			case SPAN of 
				SPANA ->
					{a,N};
				_ ->
					{b,N}
			end
		end || N <- All ],
	A = [N || {a, N} <- AB],
	B = [N || {b, N} <- AB],
	{A, B}.

search_nodes_same_process(SPAN, G) -> 
	lists:flatten([
		begin 
			{Id, {_, SPAN_Id}} = digraph:vertex(G, VD),
			case SPAN_Id of 
				SPAN -> 
					[Id];
				_ ->
					[]
			end
		end || VD <- digraph:vertices(G)]).

read_from_file_content({src_position,N,Ini,_,_}, Lines, other) ->
	read_from_file_content({src_span,N,Ini,N,Ini+1,0,0}, Lines, other);
read_from_file_content({src_span,N,Ini,NFin,Fin,_,_}, Lines, other) ->
	[LineN] = [Line ||{NL, Line} <- Lines, NL == N],
	Top =
		case NFin of 
			N -> 
				Fin - Ini;
			_ ->
				length(LineN)
		end,
	lists:sublist(LineN, Ini, Top);
read_from_file_content({src_span,N,Ini,_NFin,_Fin,_,_}, Lines, prefix) ->
	[LineN] = [Line ||{NL, Line} <- Lines, NL == N],
	lists:takewhile(fun(Char) -> Char =/= $  end, lists:sublist(LineN, Ini, length(LineN)));
read_from_file_content({src_span,N,Ini,NFin,Fin0,_,_}, Lines, call) ->
	[LineN] = [Line ||{NL, Line} <- Lines, NL == N],
	% io:format("~p\n~p\n",[{Ini,NFin,Fin0}, N]),
	{Top, Fin} =
		case NFin of 
			N -> 
				{Fin0 - Ini, Fin0 - 1};
			_ ->
				{length(LineN), length(LineN)}
		end,
	Name = lists:sublist(LineN, Ini, Top),
	% io:format("~p\n",[[lists:nth(Fin - 1, LineN), lists:nth(Fin, LineN), lists:nth(Fin + 1, LineN)]]),
	Args = 
		case lists:nth(Fin + 1, LineN) of 
			$( ->
				lists:takewhile(fun(Char) -> Char =/= $) end, lists:sublist(LineN, Fin + 1, length(LineN))) ++ ")";
			_ ->
				""
		end,
	Name ++ Args.

find_lhs_from(_, 0) ->
	"";
find_lhs_from(Lines, N) ->
	[LineN] = [Line ||{NL, Line} <- Lines, NL == N],
	% io:format("~s\n", [LineN]),
	case lists:takewhile(fun(Char) -> Char =/= $= end, LineN) of 
		LineN ->
			find_lhs_from(Lines, N - 1);
		Other ->
			Other
	end.	

extract_spans(Slice, Digraph) ->
	[ begin {V,{_,SPAN}} = digraph:vertex(Digraph, V), SPAN end 
		|| V <- digraph:vertices(Digraph), lists:member(V,Slice)].
