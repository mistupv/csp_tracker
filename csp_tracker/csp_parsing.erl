-module(csp_parsing).

%repasar que no quede ninguna sense utilitzar
-export([search_parameters/2,rewrite_vars/1,
         matching/2,replace_parameters/2,
         fake_process_name/1]).


search_parameters([$(|Tail],Acc) ->
	Parameters = extract_expressions(Tail),
	{list_to_atom(Acc),Parameters};
search_parameters([Char|Tail],Acc) ->
	search_parameters(Tail,Acc++[Char]);
search_parameters([],Acc) -> {list_to_atom(Acc),[]}.

extract_expressions([$i,$n,$t,$(|NTail]) ->
	{Num, NNtail_} = read_whole_var(NTail),
	[$)|NNtail] = NNtail_,
	case NNtail of
	     [$,|TailVars] -> 
	     	[list_to_integer(Num)|extract_expressions(TailVars)];
	     [$)|_] ->
	     	[list_to_integer(Num)];
	     [_|TailVars] ->
	     	extract_expressions(TailVars)
	end;
extract_expressions([$v,$a,$r,$_|NTail]) ->
	{Var, NNtail} = read_whole_var(NTail),
	case NNtail of
	     [$,|TailVars] -> 
	     	[list_to_atom("var_"++Var)|extract_expressions(TailVars)];
	     [$)|_] ->
	     	[list_to_atom("var_"++Var)];
	     [_|TailVars] ->
	     	extract_expressions(TailVars)
	end;
extract_expressions([]) -> [].


rewrite_vars([$_|Tail]) ->
	{Var, Ntail} = read_whole_var(Tail),
	case Var of
	     [] ->
	     	[$_|rewrite_vars(Tail)];
	     _ -> 
	       	[$v,$a,$r,$_|Var]++ rewrite_vars(Ntail)
	 end;
rewrite_vars([Char|Tail]) ->
	[Char|rewrite_vars(Tail)];
rewrite_vars([]) ->
	[].
	
	
read_whole_var([Char|Tail]) ->
	case lists:member(Char,[$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]) of
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
	%io:format("NExpr: ~p\nDict: ~p\n",[NExpr,Dict]),
	case is_list(NExpr) orelse contains_vars(NExpr) of
	     true ->
	        case NExpr of
	             {op,_,Op,E1,E2} ->
	             	reconvert_to_string(NExpr);
	             _ -> Str 
	     	end;
	     false ->
		{value,Value,_} = erl_eval:expr(NExpr,[]),
		%io:format("Value: ~p\n",[Value]),
		Value
	end.


contains_vars({op,_,_,E1,E2}) ->
	contains_vars(E1) orelse contains_vars(E2);
contains_vars({atom,_,Atom}) ->
        case atom_to_list(Atom) of
             [$v,$a,$r,$_|_] -> true;
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
replace_parameters({procRenaming,{rename,Original,Renamed},P,SPAN},Dict) ->
	{procRenaming,{rename,hd(replace_parameters_list([Original],Dict)),
		              hd(replace_parameters_list([Renamed],Dict))},
	 replace_parameters(P,Dict),SPAN};
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
	FEvent = 
	  case is_list(Event) of
	       true -> list_to_atom(Event);
	       false -> Event
	   end,	
	case [Arg_||{Par,Arg_} <- Dict,Par=:=FEvent] of
	     [Arg|_] ->
	     	Arg;
	     _ ->
	     	Event
	end.
	
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
	     [$v,$a,$r,$_|_] ->
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
fake_process_name([$-|Tail]) ->
	case Tail of
	     [$>,$_|_] -> true;
	     _ -> fake_process_name(Tail)
	end;
fake_process_name([_|Tail]) -> 
	fake_process_name(Tail).

%replace_fake_processes({prefix,SPANevent,Channels,Event,P,SPANarrow},Dict) ->
%	{prefix,SPANevent,Channels,
%	 Event,replace_fake_processes(P,Dict),SPANarrow};
%replace_fake_processes({'|~|',P1,P2,SPAN},Dict) ->
%	{'|~|',replace_fake_processes(P1,Dict),replace_fake_processes(P2,Dict),SPAN};
%replace_fake_processes({'[]',P1,P2,SPAN},Dict) ->
%	{'[]',replace_fake_processes(P1,Dict),replace_fake_processes(P2,Dict),SPAN};
%replace_fake_processes({'ifte',ConditionStr,P1,P2,SPAN1,SPAN2,SPAN3},Dict) ->
%	{'ifte',ConditionStr,replace_fake_processes(P1,Dict),
%	 replace_fake_processes(P2,Dict),SPAN1,SPAN2,SPAN3};
%replace_fake_processes({agent_call,SPAN,ProcessName,Arguments},Dict) ->
%     	 case fake_process_name(atom_to_list(ProcessName)) of
%     	      true ->
%     	      	case ets:lookup(Dict, ProcessName) of
%     	      	     [{ProcessName,{_,ProcessBody_}}] ->
%     	      	     	ets:delete(Dict, ProcessName),
%     	      	     	ProcessBody_;
%     	      	     _ ->
%     	      	     	{agent_call,SPAN,ProcessName,Arguments}
%     	      	end;
%     	      false ->
%     	      	{agent_call,SPAN,ProcessName,Arguments}
%     	 end;
%	%{agent_call,SPAN,ProcessName,Arguments};
%replace_fake_processes({sharing,{closure,Events},P1,P2,SPAN},Dict) ->
%	{sharing,{closure,Events},
%	 replace_fake_processes(P1,Dict),replace_fake_processes(P2,Dict),SPAN};
%replace_fake_processes({'|||',P1,P2,SPAN},Dict) ->
%	{'|||',replace_fake_processes(P1,Dict),replace_fake_processes(P2,Dict),SPAN};
%replace_fake_processes({procRenaming,{rename,Original,Renamed},P,SPAN},Dict) ->
%	{procRenaming,{rename,Original,Renamed},
%	 replace_fake_processes(P,Dict),SPAN};
%replace_fake_processes({'\\',P,{closure,Events},SPAN},Dict) ->
%	{'\\',replace_fake_processes(P,Dict),{closure,Events},SPAN};
%replace_fake_processes({';',PA,PB,SPAN},Dict) ->
%	{';',replace_fake_processes(PA,Dict),replace_fake_processes(PB,Dict),SPAN};
%replace_fake_processes({skip,SPAN},_) -> {skip,SPAN};
%replace_fake_processes({stop,SPAN},_) -> {stop,SPAN}.
%
%
%fake_process_name([]) -> false;
%fake_process_name([$-|Tail]) ->
%	case Tail of
%	     [$>,$_|_] -> true;
%	     _ -> fake_process_name(Tail)
%	end;
%fake_process_name([_|Tail]) -> 
%	fake_process_name(Tail).