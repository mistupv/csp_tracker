%IDEA: quant l'altra rama haja parat enviar un dont_send_my again per a que para de enviar-li ixe event. Si arriba al prefixing que li envie un event espcial diguent-li que no tinc res que oferir entonces que els altres ja decidisquen si parar el proces o no
%IDEA 2: Enrecordarme de no dibuixar el external choice o el parallelisme fins que el fills no executen algo. Aço es pot fer dins del paralleislme mateixa, la primera volta que es reba algo
-module(csp_process).

-export([first/1,loop/5,loop_root/1]).

first(Timeout) -> 
	io:format("\n-> START_TRACE\n\n"),
	Root = spawn(csp_process,loop_root,[self()]),
	spawn(csp_process,loop,[{agent_call,{src_span,0,0,0,0,0,0},'MAIN',[]},Root,-1,[],[]]),
	receive
		ok -> 	io:format("\n<- FINISH_TRACE\n");
		stopped -> 	io:format("\n<- STOPPED_TRACE (deadlock)\n")
	after
		Timeout -> io:format("\nTimeout.\n")
	end,
	exit(Root, kill),
	%loop_root(self()),
	printer!stop,
	codeserver!stop,
	ok.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Main Loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
loop_root(First) ->
        %io:format("a la espera root ~p\n",[self()]),
	receive	
		{finished,_,_} ->
			First!ok;
		{finished_skip,SPAN,GraphParent,PidSkip,_,IsFinal} ->
			print_skip(SPAN,GraphParent,PidSkip,IsFinal),
			loop_root(First);	
		{stopped,_} -> 
			First!stopped;
		{event,Event,Channels,Pid,PidPrefixing,_,_} ->
		        %io:format("Arriba ~p amb canals ~p\n",[Event,Channels]),
			SelectedChannels = select_channels(Channels),
			ChannelsString = create_channels_string(SelectedChannels),
			EventString = 
			    case ChannelsString of
			         "" -> 
					atom_to_list(Event);
				 _ -> 
				 	case atom_to_list(Event) of
				 	     [$ ,$ ,$ ,$t,$a,$u|_] ->
				 	     	atom_to_list(Event);
				 	     _ ->  
				  		atom_to_list(Event) ++ "." ++ ChannelsString
				  	end
			    end,
			ExecutedEvent = list_to_atom(EventString),
		        printer!{print,ExecutedEvent,self()},
			receive
			   {printed,ExecutedEvent} -> ok
			end,
			Pid!{executed,PidPrefixing,self(),SelectedChannels},
			receive
			     _ ->  PidPrefixing!continue
			end,
			loop_root(First)
	end.

loop(Process,PidParent,GraphParent,PendingSC,Renaming) ->
	{NState,NPendingSC,NGraphParent} = 
		case Process of
		     {finished,_,_} = FinishedState ->
		     	{FinishedState,PendingSC,GraphParent};
		     {';',PA,PB,SPAN} ->
		     	{PA,[{PB,Renaming,SPAN}|PendingSC],GraphParent};
		     {skip,SPAN} -> 
		     	{{finished_skip,SPAN},PendingSC,GraphParent};
	  	     {prefix,SPAN1,Channels,Event,ProcessPrefixing,SPAN2} ->
	  	        {NState_,NGraphParent_} =
	  	             process({prefix,SPAN1,Channels,Event,
			             ProcessPrefixing,SPAN2},
			             PidParent,GraphParent,Renaming),
	  	        {NState_, PendingSC, NGraphParent_};
		     _ ->
		        %io:format("Create_graph de ~p (~p)\n",[Process,self()]),
				printer!{create_graph,Process,GraphParent,self()},
				receive
					{created,NGraphParent_} ->
					   Res = process(Process,PidParent,NGraphParent_,Renaming),
					   %io:format("res ~p\n",[Res]),
					   {Res,
					    PendingSC,NGraphParent_}
		  		end
		end,
        case NState of
             {finished_skip,SPANSKIP} ->
             	%io:format("Envio: ~p\n",[{finished_skip,SPANSKIP,NGraphParent,self()}]),
             	IsFinal =
             	  case NPendingSC of
             	       [] -> true;
             	       _ -> false
             	  end,
             	PidParent!{finished_skip,SPANSKIP,NGraphParent,self(),self(),IsFinal},
             	receive
             	   {continue_skip,NNGraphParent} ->
             	      loop({finished,self(),[NNGraphParent]},
             	            PidParent,NNGraphParent,NPendingSC,Renaming)
             	end;
             {finished,Pid,FinishedNodes} -> 
                  case NPendingSC of
	               [{Pending,RenamingOfPending,SPANSC}|TPendingSC] ->
                           printer!{print,'   tau',self()},
                           printer!{create_graph,{';',FinishedNodes,SPANSC},-1,self()},
                           receive
			     {printed,'   tau'} -> 
			   	ok
			   end,
			   receive
			      {created,NodeSC} -> 
				   loop(Pending,PidParent,NodeSC,TPendingSC,RenamingOfPending)
			   end;
                        _ ->
                 	   PidParent!{finished,Pid,FinishedNodes}
                 end;
             {stopped,Pid} -> 
             	%io:format("Entra\n"),
             	PidParent!{stopped,Pid};
             {renamed,NProcess,NRenaming} -> 
             	loop(NProcess,PidParent,NGraphParent,NPendingSC,NRenaming);
             NProcess ->
             	%io:format("Loop (~p) from ~p to ~p \n",[self(),Process,NProcess]),
             	loop(NProcess,PidParent,NGraphParent,NPendingSC,Renaming)
        end.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process({prefix,_,Channels,Event,Process,_}=Prefixing,PidParent,GraphParent,Renaming) -> 
	ExecutedEvent = rename_event(Event,Renaming),
	prefixing_loop(PidParent,Prefixing,Process,GraphParent,
	              {event,ExecutedEvent,Channels,self(),self(),Prefixing,GraphParent},
	              Channels);
process({'|~|',PA,PB,_},_,_,_) ->
	process_choice(PA,PB,true);
process({'[]',PA,PB,_},PidParent,GraphParent,Renaming) ->
        {PA_,PB_} = random_branches(PA,PB),
	process_external_choice(PA_,PB_,PidParent,GraphParent,Renaming);
process({'ifte',Condition,PA,PB,_,_,_},_,_,_) ->
	Event = list_to_atom("   tau -> Condition Choice value "++atom_to_list(Condition)),
	printer!{print,Event,self()},
	receive
		{printed,Event} -> ok
	end,
	case Condition of
	     true -> PA;
	     false -> PB
	end;
process({agent_call,_,ProcessName,Arguments},_,_,_) ->
        Event = list_to_atom("   tau -> Call to process "++atom_to_list(ProcessName)
                                         ++printer:string_arguments(Arguments)),
	printer!{print,Event,self()},
	codeserver!{ask_code,ProcessName,Arguments,self()},
	receive
		{printed,Event} -> ok
	end,
	receive
		{code_reply,Code} -> Code
	end;
process({sharing,{closure,Events},PA,PB,_},PidParent,GraphParent,Renaming) ->
        {PA_,PB_} = random_branches(PA,PB),
	process_parallelism(PA_,PB_,Events,PidParent,GraphParent,Renaming);
process({'|||',PA,PB,_},PidParent,GraphParent,Renaming) ->
        {PA_,PB_} = random_branches(PA,PB),
	process_parallelism(PA_,PB_,[],PidParent,GraphParent,Renaming);
process({procRenaming,{rename,Original,Renamed},P,_},_,_,Renaming) ->
	{renamed,P,[{Original,Renamed}|Renaming]};
process({'\\',P,{closure,Events},_},_,_,Renaming) ->
	{renamed,P,[{Event,'   tau -> Hidding'} || Event <- Events] ++Renaming};
process({stop,_},_,_,_) ->
	printer!{print,'   tau -> STOP',self()},
	receive
		{printed,'   tau -> STOP'} -> {stopped,self()}
	end.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Prefixing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
prefixing_loop(Pid,Prefixing,Process,GraphParent,Message,Channels) ->
	%io:format("\nEnvia a ~p el missatge ~p\n",[Pid,Message]),
	Pid!Message,
	receive 
		{executed,_,Pid,SelectedChannels} ->
		        Dict = createDict(Channels,SelectedChannels),
		        %io:format("Antes: ~p\n",[Prefixing]),
		        NPrefixing = csp_parsing:replace_parameters(Prefixing,Dict),
		        %io:format("Despues: ~p\n",[NPrefixing]),
		        printer!{create_graph,NPrefixing,GraphParent,self()},
				{prefix,_,_,_,NProcess,_} = NPrefixing,
				receive
			           {created,NParent} -> 
					       Pid!{sync_info,[NParent-1]},
					       receive
					           continue -> ok 
					       end,
					       {NProcess,NParent}
				end;
       	rejected ->
		%timer:sleep(50),
			prefixing_loop(Pid,Prefixing,Process,GraphParent,Message,Channels);
		rejected_all ->
		%timer:sleep(50),
			{{stopped,self()},GraphParent}
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Internal Choice 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Controlar que no quede una unica crida a esta funció	
process_choice(PA,PB,PrintTau) ->
	random:seed(now()),
	Selected = random:uniform(2),
	case PrintTau of
	     true ->
		Event = list_to_atom("   tau -> Internal Choice. Branch "++integer_to_list(Selected)),
		printer!{print,Event,self()},
		receive
			{printed,Event} -> ok
		end;
	     false ->
	      	ok
	end, 
	case Selected of
	     1 -> PA;
	     2 -> PB
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Parallelisms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
process_parallelism(PA,PB,Events,PidParent,GraphParent,Renaming) ->
	PidA = spawn(csp_process,loop,[PA,self(),GraphParent,[],[]]),
	PidB = spawn(csp_process,loop,[PB,self(),GraphParent,[],[]]),
	%io:format("Parallelisme fill de ~p: ~p\n",[self(),{PidA,PidB}]),
	parallelism_loop(PidA,PidB,Events,PidParent,[],Renaming,{{},{}}).
	
parallelism_loop(PidA,PidB,SyncEvents,PidParent,Finished,Renaming,TemporalGraphs) ->
	case length([Fin || Fin = {_,NodesFinished} <- Finished, NodesFinished =/=[]]) of
	     2 -> 
	       {finished,self(),lists:append([NodesFinished || 
		                              {_,NodesFinished} <- Finished])};
%	       printer!{print,tick_SP,self()},
%	       receive
%		      {printed,tick_SP} -> 
%		         {finished,self(),
%		                   lists:append([NodesFinished || 
%		                                 {_,NodesFinished} <- Finished])}
%	       end;
	     _ ->
	     	case length(Finished) of
	     		  2 -> 
	     		  	{stopped,self()};
	     		  _ -> 
	     		  	%io:format("A la escolta SP ~p\n",[self()]),
					receive
					   {finished_skip,SPANSKIP,GraphParentSkip,PidSkip,PidAorB,true} -> 
					      case length([Fin || Fin = {_,NodesFinished} <- Finished, 
					      						NodesFinished =/=[]]) of
						   1 -> 
						       PidParent!{finished_skip,SPANSKIP,GraphParentSkip,
						                  PidSkip,self(),true};
						   _ -> 
						       PidParent!{finished_skip,SPANSKIP,GraphParentSkip,
						                  PidSkip,self(),false} 
					      end,
					      receive
					        {finished,PidAorB,NodesFinished} ->
					           parallelism_loop(PidA,PidB,SyncEvents,PidParent,
					                            [{PidAorB,NodesFinished}|Finished],
					                            Renaming,TemporalGraphs)
					      end;
					   {finished_skip,_,_,_,_,false} = Message ->
					      PidParent!Message,
					      parallelism_loop(PidA,PidB,SyncEvents,PidParent,
					                        Finished,Renaming,TemporalGraphs);
					   {finished,PidA,NodesFinished} ->
					      parallelism_loop(PidA,PidB,SyncEvents,PidParent,
					                        [{PidA,NodesFinished}|Finished]
					                        ,Renaming,TemporalGraphs);
					   {finished,PidB,NodesFinished} ->
					      parallelism_loop(PidA,PidB,SyncEvents,PidParent,
					                       [{PidB,NodesFinished}|Finished],
					                       Renaming,TemporalGraphs);
					   {stopped,PidA} ->
			 		      parallelism_loop(PidA,PidB,SyncEvents,PidParent,
			                       [{PidA,[]}|Finished],
			                       Renaming,TemporalGraphs);
			 		   {stopped,PidB} ->
			 		      parallelism_loop(PidA,PidB,SyncEvents,PidParent,
			                       [{PidB,[]}|Finished],
			                       Renaming,TemporalGraphs);
					   {event,_,_,PidA,_,_,_} = Message ->
					       parallelism_event(Message,PidA,PidB,SyncEvents,
					   	                  PidParent,Finished,Renaming,TemporalGraphs);
					   {event,_,_,PidB,_,_,_} = Message ->
					   	parallelism_event(Message,PidA,PidB,SyncEvents,
					   	                  PidParent,Finished,Renaming,TemporalGraphs)        
					end
	     	end 
	end.
	
parallelism_event({event,Event,Channels,Pid,PidPrefixing,Prefixing,GraphParent},PidA,PidB,
                  SyncEvents,PidParent,Finished,Renaming,TemporalGraphs) ->
	ExecutedEvent = rename_event(Event,Renaming),
	NTemporalGraphs = 
	        case Pid =:= PidA of
	             true ->
	               process_event(Event,ExecutedEvent,PidA,PidB,PidPrefixing,
	                             Channels,SyncEvents,PidParent,
	                             Prefixing,GraphParent,
	                             TemporalGraphs,PidA,PidB,
	                             Finished);
	             false ->
	               process_event(Event,ExecutedEvent,PidB,PidA,PidPrefixing,   
	                             Channels,SyncEvents,PidParent,
	                             Prefixing,GraphParent,
	                             TemporalGraphs,PidA,PidB,
	                             Finished)
	        end,          
	parallelism_loop(PidA,PidB,SyncEvents,PidParent,
                 Finished,Renaming,NTemporalGraphs).

process_event(Event,ExecutedEvent,PidA,PidB,PidPrefixingA,ChannelsA,
              SyncEvents,PidParent,PrefixingA,GraphParentA,
              TemporalGraphs,PidAOri,PidBOri,Finished) ->
%        io:format("SP ~p processa event ~p enviat per ~p\n",
%                  [self(),Event,PidA]),
	case lists:member(Event,SyncEvents) of
	     true ->
	       	NTemporalGraphs_ = remove_temporal_graph(PidA,TemporalGraphs),
        	NTemporalGraphs = create_temporal_graph(PidA,NTemporalGraphs_,PrefixingA,
                                                GraphParentA,PidAOri,PidBOri),
	        case length(Finished) of
	             1 -> 
	             	PidA!rejected_all,
			  		NTemporalGraphs;
	             _ -> 
	              	receive
	              	    {event,Event,ChannelsB,PidB,PidPrefixingB,_,_} -> 	     
	              	        case create_channels(ChannelsA,ChannelsB,[]) of
	              	             no_compatible ->
	              	                PidA!rejected,
							        PidB!rejected,
							        NTemporalGraphs;
				     			SelectedChannels ->   	              	    		  
									process_both_branches(ExecutedEvent,
									        PidA,PidPrefixingA,
									        PidB,PidPrefixingB,
									        SelectedChannels,
						                    PidParent,NTemporalGraphs)
	              	        end
	              	after 
	              	    0 -> 
	              	    	receive
	              	    	   {event,Event,ChannelsB,PidB,PidPrefixingB,_,_} ->
		              	    	case create_channels(ChannelsA,ChannelsB,[]) of
		              	            no_compatible ->
		              	                PidA!rejected,
					        			PidB!rejected,
					        			NTemporalGraphs;
					     			SelectedChannels ->   	              	    	  
										process_both_branches(ExecutedEvent,
										        PidA,PidPrefixingA,
										        PidB,PidPrefixingB,
										        SelectedChannels,
							                    PidParent,NTemporalGraphs)
		              	        end;
			                   Message ->
			                        PidA!rejected,
			                   		self()!Message,
			                   		NTemporalGraphs		                   
	              	    	end
	              	end
	        end;
   	     false -> 
   	         PidParent!{event,ExecutedEvent,ChannelsA,self(),
   	                    PidPrefixingA,PrefixingA,GraphParentA},
                 receive
                   {executed,PidPrefixingA,PidParent,SelectedChannels} -> 
                      NTemporalGraphs = remove_temporal_graph(PidA,TemporalGraphs),
                      PidA!{executed,PidPrefixingA,self(),SelectedChannels},
                      receive
                         {sync_info,_} = Message ->
                            PidParent ! Message
                      end,
                      NTemporalGraphs; 
                   rejected ->
                      PidA!rejected,
                      TemporalGraphs
                  end
	end.

process_both_branches(ExecutedEvent,PidA,PidPrefixingA,PidB,PidPrefixingB,
                      SelectedChannels,PidParent,NTemporalGraphs) ->	
	PidParent!{event,ExecutedEvent,SelectedChannels,self(),self(),{},-1},
	receive
	   {executed,_,PidParent,FinallySelectedChannels} ->
	       NNTemporalGraphs_ = remove_temporal_graph(PidA,NTemporalGraphs),
	       NNTemporalGraphs = remove_temporal_graph(PidB,NNTemporalGraphs_),
	       PidA!{executed,PidPrefixingA,self(),FinallySelectedChannels},
	       PidB!{executed,PidPrefixingB,self(),FinallySelectedChannels},
	       
	       receive
	           {sync_info,NodesA} ->
	              ok
	       end,
	       receive
	           {sync_info,NodesB} ->
	              ok
	       end,
	       PidPrefixingA!continue,
	       PidPrefixingB!continue,
	       [print_sync(NodeA,NodeB) || NodeA <- NodesA, 
		                           NodeB <- NodesB],
	       PidParent!{sync_info,NodesA ++ NodesB},
	       receive
	           continue -> ok
	       end,
	       NNTemporalGraphs;
	   rejected -> 
	     PidA!rejected,
	     PidB!rejected,
	     NTemporalGraphs;
	   rejected_all -> 
	     PidA!rejected_all,
	     PidB!rejected_all,
	     NTemporalGraphs
	end.	

print_sync(NodeA,NodeB) ->
	printer!{print_sync,NodeA,NodeB,self()},
	receive
           {printed_sync,NodeA,NodeB} ->
   	       ok
	end.
	
	
create_channels([],[],FinalChannels) -> 
	FinalChannels;
create_channels([{out,Channel}|CA],[{in,_}|CB],FinalChannels) ->
	create_channels(CA,CB,[{out,Channel}|FinalChannels]);
create_channels([{out,Channel}|CA],[{'inGuard',_,Channels}|CB],FinalChannels) ->
	case lists:member(Channel,Channels) of
	     true -> create_channels(CA,CB,[{out,Channel}|FinalChannels]);
	     false -> no_compatible
	end;
create_channels([{out,ChannelA}|CA],[{out,ChannelB}|CB],FinalChannels) ->
	case ChannelA=:=ChannelB of
	     true -> create_channels(CA,CB,[{out,ChannelA}|FinalChannels]);
	     false -> no_compatible
	end;
create_channels([{in,_}|_],[{in,_}|_],_) ->
	no_compatible;
create_channels([{in,_}|CA],[{'inGuard',Var,Channels}|CB],FinalChannels) ->
	create_channels(CA,CB,[{'inGuard',Var,Channels}|FinalChannels]);
create_channels([{'inGuard',Var,ChannelsA}|CA],[{'inGuard',_,ChannelsB}|CB],FinalChannels) ->
	Intersection = 
	   sets:tolist(sets:intersection(sets:from_list(ChannelsA),sets:from_list(ChannelsB))),
	case Intersection of
	     [] -> no_compatible;
	     _ -> 
	       create_channels(CA,CB,[{'inGuard',Var,Intersection}|FinalChannels])
	end;
create_channels(ChannelsA,ChannelsB,FinalChannels) ->
	create_channels(ChannelsB,ChannelsA,FinalChannels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   External Choices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
process_external_choice(PA,PB,PidParent,GraphParent,Renaming) -> 
	PidA = spawn(csp_process,loop,[PA,self(),GraphParent,[],[]]),
	PidB = spawn(csp_process,loop,[PB,self(),GraphParent,[],[]]),
	%io:format("External choice fill de ~p: ~p\n",[self(),{PidA,PidB}]),
	external_choice_loop(PidA,PidB,PidParent,Renaming).
	

external_choice_loop(PidA,PidB,PidParent,Renaming) ->
	%io:format("A la escolta EC ~p\n",[self()]),
	receive
	   {finished_skip,SPANSKIP,GraphParentSkip,PidSkip,Pid,true} ->
	     PidParent!{finished_skip,SPANSKIP,GraphParentSkip,PidSkip,self(),true},
	     receive
	        {finished,Pid,NodesFinished} -> 
	           finish_external_choice(NodesFinished)
	     end;
	   {finished_skip,SPANSKIP,GraphParentSkip,PidSkip,_,false} ->
	     PidParent!{finished_skip,SPANSKIP,GraphParentSkip,PidSkip,self(),false},
	     external_choice_loop(PidA,PidB,PidParent,Renaming);
	   {finished,_,NodesFinished} ->
	     finish_external_choice(NodesFinished);
	   {stopped,PidA} ->
	     one_branch_loop(PidB,PidParent,Renaming);
	   {stopped,PidB} ->
	     one_branch_loop(PidA,PidParent,Renaming);
	   {event,Event,Channels,PidA,PidPrefixing,Prefixing,GraphParent} ->
	     ExecutedEvent = rename_event(Event,Renaming),
	     PidParent!{event,ExecutedEvent,Channels,self(),PidPrefixing,Prefixing,GraphParent},
	     process_event_ec(Renaming,PidPrefixing,PidParent,PidA,
	                      PidA,PidB);
	   {event,Event,Channels,PidB,PidPrefixing,Prefixing,GraphParent} ->
	     ExecutedEvent = rename_event(Event,Renaming),
	     PidParent!{event,ExecutedEvent,Channels,self(),PidPrefixing,Prefixing,GraphParent},
	     process_event_ec(Renaming,PidPrefixing,PidParent,PidB,
	                      PidA,PidB)
	end.	
       
process_event_ec(Renaming,PidPrefixing,PidParent,Pid,PidA,PidB) ->
	     receive
	     	{executed,PidPrefixing,PidParent,SelectedChannels} -> 
	     	      Pid!{executed,PidPrefixing,self(),SelectedChannels},
                      receive
                         {sync_info,_} = Message ->
                            PidParent ! Message
                      end,
	              one_branch_loop(Pid,PidParent,Renaming);
	     	rejected -> 
	     		Pid!rejected,
	     		external_choice_loop(PidA,PidB,PidParent,Renaming);
	     	rejected_all -> 
	     		Pid!rejected_all,
	     		external_choice_loop(PidA,PidB,PidParent,Renaming)
	     end.

one_branch_loop(Pid,PidParent,Renaming) ->
	%io:format("A la escolta OP ~p (selected ~p)\n",[self(),Pid]),
	receive
	   {finished_skip,SPANSKIP,GraphParentSkip,PidSkip,Pid,true} ->
	     PidParent!{finished_skip,SPANSKIP,GraphParentSkip,PidSkip,self(),true},
	     receive 
	       {finished,Pid,NodesFinished} -> 
	          {finished,self(),NodesFinished}
	     end;
	   {finished_skip,SPANSKIP,GraphParentSkip,PidSkip,Pid,false} ->
	     PidParent!{finished_skip,SPANSKIP,GraphParentSkip,PidSkip,self(),false},
	     one_branch_loop(Pid,PidParent,Renaming);
	   {finished,Pid,NodesFinished} ->
	     {finished,self(),NodesFinished};
	   {stopped,Pid} ->
	     {stopped,self()};
	   {event,Event,Channels,Pid,PidPrefixing,Prefixing,GraphParent} ->
	     ExecutedEvent = rename_event(Event,Renaming),
	     PidParent!{event,ExecutedEvent,Channels,self(),PidPrefixing,Prefixing,GraphParent},
	     receive
		   {executed,PidPrefixing,PidParent,SelectedChannels} -> 
		        Pid!{executed,PidPrefixing,self(),SelectedChannels},
	                receive
	                   {sync_info,_} = Message ->
	                      PidParent ! Message
	                end;
	           rejected -> Pid!rejected;
	           rejected_all -> Pid!rejected_all
	     end,
	     one_branch_loop(Pid,PidParent,Renaming)	   
	end.
	     

finish_external_choice(NodesFinished) ->
       printer!{print,tick_EC,self()},
       receive
	      {printed,tick_EC} -> 
	         {finished,self(),NodesFinished}
       end.
       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Other Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
rename_event(Event,[{Event,Renamed}|TailRenaming]) ->
	rename_event(Renamed,TailRenaming);
rename_event(Event,[_|TailRenaming]) ->
	rename_event(Event,TailRenaming);
rename_event(Event,[]) -> Event.

%vore que no quede una unica cria a aquesta funció
print_skip(SPAN,GraphParent,PidSkip,IsFinal) ->	
	printer!{create_graph,{skip,SPAN},GraphParent,self()},
	NGraphParent = 
		receive
		   {created,NGraphParent_} -> NGraphParent_
		end,
	case IsFinal of
	     true -> printer!{print,'   tick',self()};
	     false -> printer!{print,'   tau',self()}
	end,
	receive
	   {printed,'   tick'} -> 
		  ok;
           {printed,'   tau'} -> 
		  ok
	end,
	PidSkip!{continue_skip,NGraphParent}.
	
remove_temporal_graph(Pid,TemporalGraphs) ->	
        case TemporalGraphs of
             {{Pid,IdAno},Other} ->
	        printer!{remove_graph_no_ided,IdAno,self()},
		receive
		    {removed,IdAno} ->
		    	{{},Other}
		end;
             {Other,{Pid,IdAno}} ->
                printer!{remove_graph_no_ided,IdAno,self()},
		receive
		    {removed,IdAno} ->
		    	{Other,{}}
		end;
             _ -> TemporalGraphs
        end.
        
create_temporal_graph(Pid,{TGraphA,TGraphB},Prefixing,GraphParent,PidA,PidB) ->	 
        case Prefixing of 
             {} ->
                {TGraphA,TGraphB};
             _ ->
		printer!{create_graph_no_ided,Prefixing,GraphParent,self()},
		receive
		    {created_no_id,IdAno} ->
		    	ok
		end,
		case Pid of
		     PidA ->
		        {{Pid,IdAno},TGraphB};
		     PidB ->
		     	{TGraphA,{Pid,IdAno}}
		end
	end.	
	
random_branches(PA,PB) ->
	random:seed(now()),
	Selected = random:uniform(2),
	case Selected of
		1 -> {PA,PB};
		2 -> {PB,PA}
	end. 

select_channels([]) ->
	[];	
select_channels([{out,Channel}|Tail]) ->
	[Channel|select_channels(Tail)];
select_channels([{'inGuard',_,ChannelsList}|Tail]) ->
        random:seed(now()),
        Selected = random:uniform(length(ChannelsList)),
	[lists:nth(Selected,ChannelsList)|select_channels(Tail)].
	
create_channels_string([]) ->
	"";
create_channels_string([Channel]) when is_integer(Channel) ->
	integer_to_list(Channel);
create_channels_string([Channel]) when is_atom(Channel) ->
	atom_to_list(Channel);
create_channels_string([Channel|Tail]) when is_integer(Channel) ->
	integer_to_list(Channel)++"."++create_channels_string(Tail);
create_channels_string([Channel|Tail]) when is_atom(Channel) ->
	atom_to_list(Channel)++"."++create_channels_string(Tail).
	
createDict([{'inGuard',Var,_}|TC],[Selected|TS]) ->
	[{list_to_atom(Var),Selected}|createDict(TC,TS)];
createDict([{in,Var}|TC],[Selected|TS]) ->
	[{Var,Selected}|createDict(TC,TS)];
createDict([{out,_}|TC],[_|TS]) ->
	createDict(TC,TS);
createDict([],[]) ->
	[].
