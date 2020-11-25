-module(csp_parsing).

% repasar que no quede ninguna sense utilitzar
-export([search_parameters/2,rewrite_vars/1,
         matching/2,replace_parameters/2,
         fake_process_name/1,
         extract_type/1]).


search_parameters("(" ++ Tail,Acc) ->
	Parameters = extract_expressions(Tail),
	{list_to_atom(Acc),Parameters};
search_parameters([Char|Tail],Acc) ->
	search_parameters(Tail,Acc++[Char]);
search_parameters([],Acc) -> {list_to_atom(Acc),[]}.

extract_expressions("int(" ++ NTail) ->
	{Num, NNtail_} = read_whole_var(NTail),
	")" ++ NNtail = NNtail_,
	case NNtail of
		"," ++ TailVars ->
			[list_to_integer(Num)|extract_expressions(TailVars)];
		")" ++ _ ->
			[list_to_integer(Num)];
		[_|TailVars] ->
			extract_expressions(TailVars)
	end;
extract_expressions("var_" ++ NTail) ->
	{Var, NNtail} = read_whole_var(NTail),
	case NNtail of
		"," ++ TailVars ->
			[list_to_atom("var_"++Var)|extract_expressions(TailVars)];
		")" ++ _ ->
			[list_to_atom("var_"++Var)];
		[_|TailVars] ->
			extract_expressions(TailVars)
	end;
extract_expressions([]) -> [].

extract_type("'dotUnitType'") ->
	[];
extract_type("'dotTupleType'([" ++ Tail) ->
	get_type_tokens(Tail);
extract_type(_) ->
	[].

get_type_tokens("'setFromTo'(" ++ Tail) ->
	{Ini,Fin,NTail} = get_ini_fin(Tail,1,-1),
	Type = 
		try 
			lists:seq(Ini,Fin)
		catch 
			_:_ -> []
		end,
	[Type|get_type_tokens(NTail)];
get_type_tokens(",'setFromTo'(" ++ Tail) ->
	{Ini,Fin,NTail} = get_ini_fin(Tail,1,-1),
	Type = 
		try 
			lists:seq(Ini,Fin)
		catch 
			_:_ -> []
		end,
	[Type|get_type_tokens(NTail)];
get_type_tokens("'setEnum'([" ++ Tail) ->
	{Type,NTail} = get_enum(Tail,[]),
	[Type|get_type_tokens(NTail)];
get_type_tokens(",'setEnum'([" ++ Tail) ->
	{Type,NTail} = get_enum(Tail,[]),
	[Type|get_type_tokens(NTail)];
get_type_tokens([_|_]) -> 
	[];
get_type_tokens([]) ->
	[].

% ['setFromTo'('int'(0),'int'(2)),'setFromTo'('int'(0),'int'(2)),'setFromTo'('int'(0),'int'(2)),'setFromTo'('int'(0),'int'(2))])

get_ini_fin("'int'(" ++ Tail,_,Fin) ->
	{InitialNumberStr,NTail} = untill_bracket(Tail),
	InitialNumber = list_to_integer(InitialNumberStr),
	get_ini_fin(NTail,InitialNumber,Fin);
get_ini_fin(",'int'(" ++ Tail,Ini,_) ->
	{FinalNumberStr,NTail} = untill_bracket(Tail),
	FinalNumber = list_to_integer(FinalNumberStr),
	get_ini_fin(NTail,Ini,FinalNumber);
get_ini_fin([_|Tail],Ini,Fin) ->
	{Ini,Fin,Tail};
get_ini_fin([],Ini,Fin) ->
	{Ini,Fin,[]}.

get_enum("'int'(" ++ Tail,Acc) ->
	{NumberStr,NTail} = untill_bracket(Tail),
	Number = list_to_integer(NumberStr),
	get_enum(NTail,[Number|Acc]);
get_enum(",'int'(" ++ Tail,Acc) ->
	{NumberStr,NTail} = untill_bracket(Tail),
	Number = list_to_integer(NumberStr),
	get_enum(NTail,[Number|Acc]);
get_enum("'" ++ Tail,Acc) ->
	{AtomStr,NTail} = untill_colon(Tail),
	Atom = list_to_atom(AtomStr),
	get_enum(NTail,[Atom|Acc]);
get_enum([_Other|Tail],Acc) ->
	{Acc,Tail};
get_enum([],Acc) ->
	{Acc,[]}.

untill_bracket(String) ->
	untill_char($),String,[]).

untill_colon(String) ->
	untill_char($,,String,[]).

untill_char(Char,[Char|Tail],Acc) ->
	{Acc,Tail};
untill_char(Char,[OtherChar|Tail],Acc) ->
	untill_char(Char,Tail,[OtherChar|Acc]);
untill_char(_,[],Acc) ->
	{Acc,[]}.


rewrite_vars("_" ++ Tail) ->
	{Var, Ntail} = read_whole_var(Tail),
	case Var of
	     [] ->
	     	"_" ++ rewrite_vars(Tail);
	     _ -> 
	       	"var_" ++ Var ++ rewrite_vars(Ntail)
	 end;
rewrite_vars("!=(" ++ Tail) ->
	First = string:str(Tail,"\""),
	NTail = string:sub_string(Tail, First), 
	Args = string:sub_string(Tail,1,First - 2),
	[Arg1 , Arg2] = string:tokens(Args, ","), 
	rewrite_vars(Arg1) ++ "/=" ++ rewrite_vars(Arg2) 
	++ rewrite_vars(NTail);
rewrite_vars([Char|Tail]) ->
	[Char|rewrite_vars(Tail)];
rewrite_vars([]) ->
	[].
	
	
read_whole_var([Char|Tail]) ->
	case lists:member(Char,"0123456789") of
	     true -> 
	     	{RVar,RTail} = read_whole_var(Tail),
	        {[Char|RVar],RTail};
	     false ->
	     	{[],[Char|Tail]}
	end;
read_whole_var([]) -> {[],[]}.

get_value(Str,Dict) ->
	Expr = read_expression(Str),
	NExpr = replace_vars(Expr,Dict),
	% io:format("Expr: ~p\nNExpr: ~p\nDict: ~p\n",[Expr,NExpr,Dict]),
	% io:format("Expr: ~p\n",[Expr]),
	% io:format("Self: ~p\n",[self()]),
	case is_list(NExpr) orelse contains_vars(NExpr) of
	     true ->
	        case NExpr of
	             {op,_,_Op,_E1,_E2} ->
	             	reconvert_to_string(NExpr);
	             _ -> Str 
	     	end;
	     false ->
	     	NNExpr = 
	     		case NExpr of 
	     			{op,Op,LINE,E1,E2} ->
	     				{op,Op,LINE,convert_erlang(E1),convert_erlang(E2)};
	     			_ ->
	     				NExpr
	     		end,
	     	% io:format("Expr: ~p\nNExpr: ~p\nDict: ~p\n",[Expr,NExpr,Dict]),
	     	% io:format("NNExpr: ~p\n",[NNExpr]),
	     	try 
				{value,Value,_} = erl_eval:expr(NNExpr,[]),
				% io:format("Value: ~p\n",[Value]),
				Value
			catch
				_:_ -> NNExpr
			end
	end.


contains_vars({op,_,_,E1,E2}) ->
	contains_vars(E1) orelse contains_vars(E2);
contains_vars({atom,_,Atom}) ->
	case atom_to_list(Atom) of
		"var_" ++ _ -> true;
		_ -> false
	end;
contains_vars(_) ->
	false.
	
reconvert_to_string({op,_,Op,E1,E2}) ->
	reconvert_to_string(E1) ++ atom_to_list(Op) ++ reconvert_to_string(E2);
reconvert_to_string({atom,_,Atom}) ->
	atom_to_list(Atom);
reconvert_to_string({integer,_,Int}) ->
	integer_to_list(Int).

read_expression(ExprStr) when is_list(ExprStr) ->
        %io:format("ExprStr: ~p\n",[ExprStr]),
	case erl_scan:string(ExprStr++".") of
	{ok, Toks, _} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok, _Term} ->
		    hd(_Term);
		_Err ->
		    ExprStr%{error, parse_error}
	    end;
	_Err ->
	    ExprStr%{error, parse_error}
    end;
read_expression(ExprAt) when is_atom(ExprAt) ->
	read_expression(atom_to_list(ExprAt));
read_expression(ExprStr) -> ExprStr.
    
    
replace_parameters({prefix,SPANevent,Channels,Event,P,SPANarrow},Dict) ->
	{prefix,SPANevent,replace_channels(Channels,Dict),
	 replace_event(Event,Dict),
	 replace_parameters(P,Dict),SPANarrow};
replace_parameters({'|~|',P1,P2,SPAN},Dict) ->
	{'|~|',replace_parameters(P1,Dict),replace_parameters(P2,Dict),SPAN};
replace_parameters({'[]',P1,P2,SPAN},Dict) ->
	{'[]',replace_parameters(P1,Dict),replace_parameters(P2,Dict),SPAN};
replace_parameters({'ifte',ConditionStr,P1,P2,SPAN1,SPAN2,SPAN3},Dict) ->
	%reescriure abans <= -> =< i != -> =/=
	{'ifte',get_value(ConditionStr,Dict),replace_parameters(P1,Dict),
	 replace_parameters(P2,Dict),SPAN1,SPAN2,SPAN3};
replace_parameters({agent_call,SPAN,ProcessName,Arguments},Dict) ->
	{agent_call,SPAN,ProcessName,replace_parameters_list(Arguments,Dict)};
replace_parameters({sharing,{closure,Events},P1,P2,SPAN},Dict) ->
	{sharing,{closure,replace_parameters_list(Events,Dict)},
	 replace_parameters(P1,Dict),replace_parameters(P2,Dict),SPAN};
replace_parameters({'|||',P1,P2,SPAN},Dict) ->
	{'|||',replace_parameters(P1,Dict),replace_parameters(P2,Dict),SPAN};
replace_parameters({procRenaming,Renamings,P,SPAN},Dict) ->
	NRenamings0 = 
		case Renamings of 
			{rename,Original,Renamed} -> 
				[{rename,hd(replace_parameters_list([Original],Dict)),
		              hd(replace_parameters_list([Renamed],Dict))}];
		    _ ->
		    	[{rename,hd(replace_parameters_list([Original],Dict)),
		              hd(replace_parameters_list([Renamed],Dict))} 
		         || {rename,Original,Renamed} <- Renamings]
		end,
	NP0 = replace_parameters(P,Dict),
	{NRenamings, NP} = 
		case NP0 of 
			{procRenaming,Renamings_NP0,P_NP0,_} ->
				{NRenamings0 ++ Renamings_NP0,P_NP0};
			_ ->
				{NRenamings0,NP0}
		end,
	{procRenaming,NRenamings,NP,SPAN};
replace_parameters({'\\',P,{closure,Events},SPAN},Dict) ->
	{'\\',replace_parameters(P,Dict),{closure,replace_parameters_list(Events,Dict)},SPAN};
replace_parameters({';',PA,PB,SPAN},Dict) ->
	{';',replace_parameters(PA,Dict),replace_parameters(PB,Dict),SPAN};
replace_parameters({skip,SPAN},_) -> {skip,SPAN};
replace_parameters({stop,SPAN},_) -> {stop,SPAN}.
	
replace_parameters_list([Event=[_|_]|Events],Dict) ->
	[get_value(Event,Dict)|replace_parameters_list(Events,Dict)];	
replace_parameters_list([Event|Events],Dict) ->
	[replace_event(Event,Dict)|replace_parameters_list(Events,Dict)];
replace_parameters_list([],_) -> [].
    
replace_vars({op,LINE,Op,E1,E2},Dict) ->
	{op,LINE,Op,replace_vars(E1,Dict),replace_vars(E2,Dict)};
replace_vars({call,_,{atom,_,int},[{integer,LINE,Int}]},_) ->
	{integer,LINE,Int};
replace_vars({atom,LINE,Atom},Dict) ->
	NEvent = replace_event(Atom,Dict),
	case is_integer(NEvent) of
	     false ->
	     	{atom,LINE,NEvent};
	     true -> 
	     	{integer,LINE,NEvent}
	end;
replace_vars(Other,Dict) -> replace_event(Other,Dict).
	
replace_event(Event,Dict)  ->
	% io:format("Replace event ~p\n",[Event]),
	FEvent = 
	  case is_list(Event) of
	       true -> list_to_atom(Event);
	       false -> 
	       		case Event of 
	       			{Op,{E1,E2}} ->
	       				Replaced = get_value({op,1,Op,E1,E2},Dict),
	       				% io:format("Replaced: ~p\n",[Replaced]),
	       				Replaced;
	       			_ -> 
	       				Event
	       		end
	   end,	
	Res =
		case [Arg_||{Par,Arg_} <- Dict,Par=:=FEvent] of
		     [Arg|_] ->
		     	Arg;
		     _ ->
		     	FEvent
		end,
	%io:format("Result: ~p\n",[Res]),
	Res.
	
convert_erlang(X) when is_integer(X) ->
	{integer,1,X};
convert_erlang(X) when is_atom(X) ->
	{atom,1,X};
convert_erlang(X) ->
	X.

replace_channels([{in,Event}|Tail],Dict) ->
	[{in,replace_event(Event,Dict)}|replace_channels(Tail,Dict)];
replace_channels([{'inGuard',Var,List}|Tail],Dict) ->
	[{'inGuard',replace_event(Var,Dict),List}|replace_channels(Tail,Dict)];
replace_channels([{out,Event}|Tail],Dict) ->
	[{out,replace_event(Event,Dict)}|replace_channels(Tail,Dict)];
replace_channels([],_) -> [].


matching([Par|Pars],[Arg|Args]) when is_atom(Par) ->
	ParStr = atom_to_list(Par),
	case ParStr of
		"var_" ++ _ ->
			matching(Pars,Args);
		_ ->
			case Par == Arg of
				true -> matching(Pars,Args);
				false -> false
			end
	end;
matching([Par|Pars],[Par|Args]) when is_integer(Par) ->
	matching(Pars,Args);
matching([Par|_],[_|_]) when is_integer(Par) ->
	false;
matching([],[]) -> true;
matching(_,_) -> false.


fake_process_name([]) -> false;
fake_process_name("-" ++ Tail) ->
	case Tail of
	     ">_" ++ _ -> true;
	     _ -> fake_process_name(Tail)
	end;
fake_process_name([_|Tail]) -> 
	fake_process_name(Tail).
