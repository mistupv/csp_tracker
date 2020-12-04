%IDEA: quant l'altra rama haja parat enviar un dont_send_my again per a que para de enviar-li ixe event. Si arriba al prefixing que li envie un event espcial diguent-li que no tinc res que oferir entonces que els altres ja decidisquen si parar el proces o no
%IDEA 2: Enrecordarme de no dibuixar el external choice o el parallelisme fins que el fills no executen algo. Aço es pot fer dins del paralleislme mateixa, la primera volta que es reba algo
-module(csp_process).

-export([
  first/3,
  start_process/5,
  loop_root/1]).

first(FirstProcess, Timeout, NoOutput) ->
  print_message("\n-> START_TRACE\n\n", NoOutput),
  register_worker_pids_table(),
  Root = spawn(csp_process, loop_root, [get_self()]),
  spawn(csp_process, start_process, [{agent_call, {src_span, 0, 0, 0, 0, 0, 0}, FirstProcess, []}, Root, -1, [], []]),
  FinishReason =
    receive
      ok -> finished;
      stopped -> deadlock
    after Timeout -> timeout
    end,
  print_message(io_lib:format("~n<- TRACE_END (~p)~n", [FinishReason]), NoOutput),
  exit(Root, kill),
  kill_worker_pids_and_delete_table(),
  %loop_root(get_self()),
  Steps = printer:get_steps(),
  InfoGraph =
    try
      printer:get_info_graph()
    catch
      throw:timeout -> {{{0, 0, 0, erlang:monotonic_time()}, "", ""}, {[], []}}
    end,
  csp_util:stop(printer),
  {InfoGraph, FinishReason, Steps}.


register_worker_pids_table() ->
  ets:new(worker_pids, [bag,public,named_table]).

kill_worker_pids_and_delete_table() ->
  Killed = lists:sum([
    case is_process_alive(Pid) of
      true -> exit(Pid, kill), 1;
      false -> 0
    end || {pid, Pid} <- ets:lookup(worker_pids, pid)]),
  ets:delete(worker_pids),
  ProcessesAlive = [Pid || Pid <- processes(), is_process_alive(Pid)],
  io:format("There are ~p processes running, ~p have been killed.~n",
    [length(ProcessesAlive), Killed]).

register_worker_pid() ->
  ets:insert(worker_pids, {pid, self()}).


print_message(Msg, NoOutput) ->
  case NoOutput of
    false ->
      io:format(Msg);
    true ->
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Main Loops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop_root(First) ->
  % io:format("a la espera root ~p\n",[get_self()]),
  receive
    {finished, _, _} ->
      First ! ok;
    {finished_skip, SPAN, GraphParent, PidSkip, _, IsFinal} ->
      print_skip(SPAN, GraphParent, PidSkip, IsFinal),
      loop_root(First);
    {stopped, _} ->
      First ! stopped;
    {event, Event, Channels, Pid, PidPrefixing, _, _} ->
      % io:format("Llega evento ~p\n",[Event]),
      SelectedChannels = select_channels(Channels, Event),
      % io:format("Arriba ~p amb canals ~p\n",[Event,Channels]),
      ChannelsString = create_channels_string(SelectedChannels),
      EventString =
        case ChannelsString of
          "" ->
            atom_to_list(Event);
          _ ->
            case atom_to_list(Event) of
              "   tau" ++ _ ->
                atom_to_list(Event);
              _ ->
                atom_to_list(Event) ++ "." ++ ChannelsString
            end
        end,
      ExecutedEvent = list_to_atom(EventString),
      printer:print(ExecutedEvent),
      Pid ! {executed, PidPrefixing, get_self(), SelectedChannels},
      receive
        _ -> PidPrefixing ! continue
      end,
      loop_root(First)
  end.

start_process(Process, PidParent, GraphParent, PendingSC, Renaming) ->
  register_worker_pid(),
  loop(Process, PidParent, GraphParent, PendingSC, Renaming).
loop(Process, PidParent, GraphParent, PendingSC, Renaming) ->
  {NState, NPendingSC, NGraphParent} =
    case Process of
      {finished, _, _} = FinishedState ->
        {FinishedState, PendingSC, GraphParent};
      {';', PA, PB, SPAN} ->
        {PA, [{PB, Renaming, SPAN} | PendingSC], GraphParent};
      {skip, SPAN} ->
        {{finished_skip, SPAN}, PendingSC, GraphParent};
      {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN2} ->
        {NState_, NGraphParent_} =
          process({prefix, SPAN1, Channels, Event,
            ProcessPrefixing, SPAN2},
            PidParent, GraphParent, Renaming),
        {NState_, PendingSC, NGraphParent_};
      _ ->
        NGraphParent_ = printer:create_graph(Process, GraphParent),
        Res = process(Process, PidParent, NGraphParent_, Renaming),
        {Res, PendingSC, NGraphParent_}
    end,
  case NState of
    {finished_skip, SPANSKIP} ->
      % io:format("Envio: ~p\n",[{finished_skip,SPANSKIP,NGraphParent,get_self()}]),
      IsFinal =
        case NPendingSC of
          [] -> true;
          _ -> false
        end,
      PidParent ! {finished_skip, SPANSKIP, NGraphParent, get_self(), get_self(), IsFinal},
      receive
        {continue_skip, NNGraphParent} ->
          loop({finished, get_self(), [NNGraphParent]},
            PidParent, NNGraphParent, NPendingSC, Renaming)
      end;
    {finished, Pid, FinishedNodes} ->
      case NPendingSC of
        [{Pending, RenamingOfPending, SPANSC} | TPendingSC] ->
          printer:print('   tau'),
          NodeSC = printer:create_graph({';', FinishedNodes, SPANSC}, -1),
          loop(Pending, PidParent, NodeSC, TPendingSC, RenamingOfPending);
        _ ->
          PidParent ! {finished, Pid, FinishedNodes}
      end;
    {stopped, Pid} ->
      %io:format("Entra\n"),
      PidParent ! {stopped, Pid};
    {renamed, NProcess, NRenaming} ->
      loop(NProcess, PidParent, NGraphParent, NPendingSC, NRenaming);
    NProcess ->
      %io:format("Loop (~p) from ~p to ~p \n",[get_self(),Process,NProcess]),
      loop(NProcess, PidParent, NGraphParent, NPendingSC, Renaming)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process({prefix, _, Channels, Event, Process, _} = Prefixing, PidParent, GraphParent, Renaming) ->
  ExecutedEvent = rename_event(Event, Renaming),
  % io:format("\tProcess: ~p\n", [Channels]),
  % io:format("Prefixing in ~p: ~p\n", [self(), {Channels,Event}]),
  prefixing_loop(PidParent, Prefixing, Process, GraphParent,
    {event, ExecutedEvent, Channels, get_self(), get_self(), Prefixing, GraphParent},
    Channels);
process({'|~|', PA, PB, _}, _, _, _) ->
  process_choice(PA, PB, true);
process({'[]', PA, PB, _}, PidParent, GraphParent, Renaming) ->
  {PA_, PB_} = random_branches(PA, PB),
  process_external_choice([PA_, PB_], PidParent, GraphParent, Renaming);
% process_external_choice(PA_,PB_,PidParent,GraphParent,Renaming);
process({'ifte', Condition, PA, PB, _, _, _}, _, _, _) ->
  Event = list_to_atom("   tau -> Condition Choice value " ++ atom_to_list(Condition)),
  printer:print(Event),
  case Condition of
    true -> PA;
    false -> PB
  end;
process({agent_call, _, ProcessName, Arguments}, _, _, _) ->
  Event = list_to_atom("   tau -> Call to process " ++ atom_to_list(ProcessName)
    ++ printer:string_arguments(Arguments)),
  printer:print(Event),
  _Code = codeserver:ask_code(ProcessName, Arguments);
process({sharing, {closure, Events}, PA, PB, _}, PidParent, GraphParent, Renaming) ->
  {PA_, PB_} = random_branches(PA, PB),
  process_parallelism(PA_, PB_, Events, PidParent, GraphParent, Renaming);
process({'|||', PA, PB, _}, PidParent, GraphParent, Renaming) ->
  {PA_, PB_} = random_branches(PA, PB),
  process_parallelism(PA_, PB_, [], PidParent, GraphParent, Renaming);
process({procRenaming, ListRenamings, P, _}, _, _, Renaming) ->
  {renamed, P, [ListRenamings | Renaming]};
process({'\\', P, {closure, Events}, _}, _, _, Renaming) ->
  {renamed, P, [[{rename, Event, '   tau -> Hidding'}] || Event <- Events] ++ Renaming};
process({stop, _}, _, _, _) ->
  case printer:print('   tau -> STOP') of
    ok -> {stopped, get_self()};
    Error -> throw(Error)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Prefixing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prefixing_loop(Pid, Prefixing, Process, GraphParent, Message, Channels) ->
  Pid ! Message,
  receive
    {executed, _, Pid, SelectedChannels} ->
      {event, ExecutedEvent, _, _, _, _, _} = Message,
      Dict = createDict(Channels, SelectedChannels, ExecutedEvent),
      NPrefixing = csp_parsing:replace_parameters(Prefixing, Dict),
      {prefix,_ , _, _, NProcess, _} = NPrefixing,
      NParent = printer:create_graph({renamed_event, ExecutedEvent, NPrefixing}, GraphParent),
      Pid ! {sync_info, [NParent - 1]},
      receive
        continue -> ok
      end,
      {NProcess, NParent};
    rejected ->
      %timer:sleep(50),
      prefixing_loop(Pid, Prefixing, Process, GraphParent, Message, Channels);
    rejected_all ->
      %timer:sleep(50),
      {{stopped, get_self()}, GraphParent}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Internal Choice 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Controlar que no quede una unica crida a esta funció	
process_choice(PA, PB, PrintTau) ->
  Selected = rand:uniform(2),
  case PrintTau of
    true ->
      Event = list_to_atom("   tau -> Internal Choice. Branch " ++ integer_to_list(Selected)),
      printer:print(Event);
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

process_parallelism(PA, PB, Events, PidParent, GraphParent, Renaming) ->
  PidA = spawn(csp_process, start_process, [PA, get_self(), GraphParent, [], []]),
  PidB = spawn(csp_process, start_process, [PB, get_self(), GraphParent, [], []]),
  % io:format("Parallelisme fill de ~p: ~p\n",[get_self(),{PidA,PidB}]),
  parallelism_loop(PidA, PidB, Events, PidParent, [], Renaming, {{}, {}}).

parallelism_loop(PidA, PidB, SyncEvents, PidParent, Finished, Renaming, TemporalGraphs) ->
  % io:format("LOOP Parallelisme fill de ~p: ~p\n",[get_self(),{PidA,PidB}]),
  case length([Fin || Fin = {_, NodesFinished} <- Finished, NodesFinished =/= []]) of
    2 ->
      % io:format("FIN Parallelisme fill de ~p: ~p\n",[get_self(),{PidA,PidB}]),
      {finished, get_self(), lists:append([NodesFinished ||
        {_, NodesFinished} <- Finished])};
    _ ->
      case length(Finished) of
        2 ->
          {stopped, get_self()};
        _ ->
          % io:format("A la escolta SP ~p\n",[get_self()]),
          receive
            {finished_skip, SPANSKIP, GraphParentSkip, PidSkip, PidAorB, true} ->
              _Send =
                case length([Fin || Fin = {_, NodesFinished} <- Finished,
                  NodesFinished =/= []]) of
                  1 ->
                    PidParent ! {finished_skip, SPANSKIP, GraphParentSkip,
                      PidSkip, get_self(), true};
                  _ ->
                    PidParent ! {finished_skip, SPANSKIP, GraphParentSkip,
                      PidSkip, get_self(), false}
                end,
              receive
                {finished, PidAorB, NodesFinished} ->
                  parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                    [{PidAorB, NodesFinished} | Finished],
                    Renaming, TemporalGraphs);
                Other ->
                  self() ! Other,
                  parallelism_loop(PidA, PidB, SyncEvents, PidParent, Finished, Renaming, TemporalGraphs)
              end;
            {finished_skip, _, _, _, _, false} = Message ->
              PidParent ! Message,
              parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                Finished, Renaming, TemporalGraphs);
            {finished, PidA, NodesFinished} ->
              parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                [{PidA, NodesFinished} | Finished]
                , Renaming, TemporalGraphs);
            {finished, PidB, NodesFinished} ->
              parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                [{PidB, NodesFinished} | Finished],
                Renaming, TemporalGraphs);
            {stopped, PidA} ->
              parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                [{PidA, []} | Finished],
                Renaming, TemporalGraphs);
            {stopped, PidB} ->
              parallelism_loop(PidA, PidB, SyncEvents, PidParent,
                [{PidB, []} | Finished],
                Renaming, TemporalGraphs);
            {event, _, _, PidA, _, _, _} = Message ->
              parallelism_event(Message, PidA, PidB, SyncEvents,
                PidParent, Finished, Renaming, TemporalGraphs);
            {event, _, _, PidB, _, _, _} = Message ->
              parallelism_event(Message, PidA, PidB, SyncEvents,
                PidParent, Finished, Renaming, TemporalGraphs)
          end
      end
  end.

parallelism_event({event, Event, Channels, Pid, PidPrefixing, Prefixing, GraphParent}, PidA, PidB,
    SyncEvents, PidParent, Finished, Renaming, TemporalGraphs) ->
  % io:format("\tparallelism: ~p\n", [Channels]),
  % io:format("Event Parallelisme ~p: ~p\n",[{PidA,PidB}, Event]),
  ExecutedEvent = rename_event(Event, Renaming),
  NTemporalGraphs =
    case Pid =:= PidA of
      true ->
        process_event(Event, ExecutedEvent, PidA, PidB, PidPrefixing,
          Channels, SyncEvents, PidParent,
          Prefixing, GraphParent,
          TemporalGraphs, PidA, PidB,
          Finished);
      false ->
        process_event(Event, ExecutedEvent, PidB, PidA, PidPrefixing,
          Channels, SyncEvents, PidParent,
          Prefixing, GraphParent,
          TemporalGraphs, PidA, PidB,
          Finished)
    end,
  parallelism_loop(PidA, PidB, SyncEvents, PidParent,
    Finished, Renaming, NTemporalGraphs).

process_event(Event, ExecutedEvent, PidA, PidB, PidPrefixingA, ChannelsA,
    SyncEvents, PidParent, PrefixingA, GraphParentA,
    TemporalGraphs, PidAOri, PidBOri, Finished) ->
  % io:format("SP ~p processa event ~p enviat per ~p\n",
  %           [get_self(),Event,PidA]),
  case lists:member(Event, SyncEvents) of
    true ->
      NTemporalGraphs_ = remove_temporal_graph(PidA, TemporalGraphs),
      NTemporalGraphs = create_temporal_graph(PidA, NTemporalGraphs_, PrefixingA,
        GraphParentA, PidAOri, PidBOri),
      case length(Finished) of
        1 ->
          PidA ! rejected_all,
          NTemporalGraphs;
        _ ->
          receive
            {event, Event, ChannelsB, PidB, PidPrefixingB, _, _} ->
              % io:format("\t\tChannelsA: ~p\n\t\tChannelsB: ~p\n\t\tCreate: ~p\n",[ChannelsA,ChannelsB,create_channels(ChannelsA,ChannelsB,[])]),
              case create_channels(ChannelsA, ChannelsB, []) of
                no_compatible ->
                  PidA ! rejected,
                  PidB ! rejected,
                  NTemporalGraphs;
                SelectedChannels ->
                  process_both_branches(ExecutedEvent,
                    PidA, PidPrefixingA,
                    PidB, PidPrefixingB,
                    SelectedChannels,
                    PidParent, NTemporalGraphs)
              end
          after
            0 ->
              receive
                {event, Event, ChannelsB, PidB, PidPrefixingB, _, _} ->
                  % io:format("\t\tChannelsA: ~p\n\t\tChannelsB: ~p\n\t\tCreate: ~p\n",[ChannelsA,ChannelsB,create_channels(ChannelsA,ChannelsB,[])]),
                  case create_channels(ChannelsA, ChannelsB, []) of
                    no_compatible ->
                      PidA ! rejected,
                      PidB ! rejected,
                      NTemporalGraphs;
                    SelectedChannels ->
                      process_both_branches(ExecutedEvent,
                        PidA, PidPrefixingA,
                        PidB, PidPrefixingB,
                        SelectedChannels,
                        PidParent, NTemporalGraphs)
                  end;
                Message ->
                  PidA ! rejected,
                  get_self() ! Message,
                  NTemporalGraphs
              end
          end
      end;
    false ->
      PidParent ! {event, ExecutedEvent, ChannelsA, get_self(),
        PidPrefixingA, PrefixingA, GraphParentA},
      receive
        {executed, PidPrefixingA, PidParent, SelectedChannels} ->
          NTemporalGraphs = remove_temporal_graph(PidA, TemporalGraphs),
          PidA ! {executed, PidPrefixingA, get_self(), SelectedChannels},
          receive
            {sync_info, _} = Message ->
              PidParent ! Message
          end,
          NTemporalGraphs;
        rejected ->
          PidA ! rejected,
          TemporalGraphs
      end
  end.

process_both_branches(ExecutedEvent, PidA, PidPrefixingA, PidB, PidPrefixingB,
    SelectedChannels, PidParent, NTemporalGraphs) ->
  PidParent ! {event, ExecutedEvent, SelectedChannels, get_self(), get_self(), {}, -1},
  receive
    {executed, _, PidParent, FinallySelectedChannels} ->
      NNTemporalGraphs_ = remove_temporal_graph(PidA, NTemporalGraphs),
      NNTemporalGraphs = remove_temporal_graph(PidB, NNTemporalGraphs_),
      PidA ! {executed, PidPrefixingA, get_self(), FinallySelectedChannels},
      PidB ! {executed, PidPrefixingB, get_self(), FinallySelectedChannels},

      receive
        {sync_info, NodesA} ->
          ok
      end,
      receive
        {sync_info, NodesB} ->
          ok
      end,
      PidPrefixingA ! continue,
      PidPrefixingB ! continue,
      [printer:print_sync(NodeA, NodeB) || NodeA <- NodesA, NodeB <- NodesB],
      PidParent ! {sync_info, NodesA ++ NodesB},
      receive
        continue -> ok
      end,
      NNTemporalGraphs;
    rejected ->
      PidA ! rejected,
      PidB ! rejected,
      NTemporalGraphs;
    rejected_all ->
      PidA ! rejected_all,
      PidB ! rejected_all,
      NTemporalGraphs
  end.


create_channels([], [], FinalChannels) ->
  lists:reverse(FinalChannels);
create_channels([{out, Channel} | CA], [{in, _} | CB], FinalChannels) ->
  create_channels(CA, CB, [{out, Channel} | FinalChannels]);
create_channels([{out, Channel} | CA], [{'inGuard', _, Channels} | CB], FinalChannels) ->
  case lists:member(Channel, Channels) of
    true -> create_channels(CA, CB, [{out, Channel} | FinalChannels]);
    false -> no_compatible
  end;
create_channels([{out, ChannelA} | CA], [{out, ChannelB} | CB], FinalChannels) ->
  case ChannelA =:= ChannelB of
    true -> create_channels(CA, CB, [{out, ChannelA} | FinalChannels]);
    false -> no_compatible
  end;
create_channels([{in, _} | _], [{in, _} | _], _) ->
  no_compatible;
create_channels([{in, _} | CA], [{'inGuard', Var, Channels} | CB], FinalChannels) ->
  create_channels(CA, CB, [{'inGuard', Var, Channels} | FinalChannels]);
create_channels([{'inGuard', Var, ChannelsA} | CA], [{'inGuard', _, ChannelsB} | CB], FinalChannels) ->
  Intersection =
    sets:to_list(sets:intersection(sets:from_list(ChannelsA), sets:from_list(ChannelsB))),
  case Intersection of
    [] -> no_compatible;
    _ ->
      create_channels(CA, CB, [{'inGuard', Var, Intersection} | FinalChannels])
  end;
create_channels(ChannelsA, ChannelsB, FinalChannels) ->
  create_channels(ChannelsB, ChannelsA, FinalChannels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   External Choices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   External Choices General
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


process_external_choice(PList0, PidParent, GraphParent, Renaming) ->
  PList =
    % From: http://stackoverflow.com/a/8820501/4162959
  [X || {_, X} <- lists:sort(
    [{rand:uniform(), P} || P <- PList0])],
  PidList =
    [spawn(csp_process, start_process, [P, get_self(), GraphParent, [], []])
      || P <- PList],
  external_choice_loop(PidList, PidParent, Renaming).


external_choice_loop(PidList, PidParent, Renaming) ->
  % io:format("External choice ~p -> ~p\n", [self(), PidList]),
  receive
    {finished_skip, SPANSKIP, GraphParentSkip, PidSkip, Pid, true} ->
      case lists:member(Pid, PidList) of
        true ->
          PidParent !
            {finished_skip,
              SPANSKIP, GraphParentSkip,
              PidSkip, get_self(), true},
          receive
            {finished, Pid, NodesFinished} ->
              case PidList of
                [Pid] ->
                  {finished, get_self(), NodesFinished};
                _ ->
                  finish_external_choice(NodesFinished)
              end
          end;
        false ->
          external_choice_loop(PidList, PidParent, Renaming)
      end;
    {finished_skip, SPANSKIP, GraphParentSkip, PidSkip, Pid, false} ->
      case lists:member(Pid, PidList) of
        true ->
          PidParent !
            {finished_skip,
              SPANSKIP, GraphParentSkip,
              PidSkip, get_self(), false},
          external_choice_loop(PidList, PidParent, Renaming);
        false ->
          external_choice_loop(PidList, PidParent, Renaming)
      end;
    {finished, Pid, NodesFinished} ->
      case lists:member(Pid, PidList) of
        true ->
          case PidList of
            [Pid] ->
              {finished, get_self(), NodesFinished};
            _ ->
              finish_external_choice(NodesFinished)
          end;
        false ->
          external_choice_loop(PidList, PidParent, Renaming)
      end;
    {stopped, Pid} ->
      case lists:member(Pid, PidList) of
        true ->
          NPidList = PidList -- [Pid],
          case NPidList of
            [] ->
              {stopped, get_self()};
            _ ->
              external_choice_loop(NPidList, PidParent, Renaming)
          end;
        false ->
          external_choice_loop(PidList, PidParent, Renaming)
      end;
    {event, Event, Channels, Pid, PidPrefixing, Prefixing, GraphParent} ->
      % io:format("External choice ~p -> received event ~p\n", [self(), {Event,Pid}]),
      case lists:member(Pid, PidList) of
        true ->
          ExecutedEvent = rename_event(Event, Renaming),
          % io:format("External choice ~p -> envia a parent ~p\n", [self(), {Event,PidParent}]),
          PidParent !
            {event,
              ExecutedEvent, Channels, get_self(),
              PidPrefixing, Prefixing, GraphParent},
          process_event_ec(
            Renaming, PidPrefixing, PidParent, Pid, PidList);
        false ->
          external_choice_loop(PidList, PidParent, Renaming)
      end
  end.

process_event_ec(Renaming, PidPrefixing, PidParent, Pid, PidList) ->
  % io:format("Receiving event ~p\n", [self()]),
  receive
    {executed, PidPrefixing, PidParent, SelectedChannels} ->
      Pid ! {executed, PidPrefixing, get_self(), SelectedChannels},
      receive
        {sync_info, _} = Message ->
          PidParent ! Message
      end,
      % io:format("One process ~p -> ~p\n", [self(), Pid]),
      external_choice_loop([Pid], PidParent, Renaming);
    rejected ->
      Pid ! rejected,
      external_choice_loop(PidList, PidParent, Renaming);
    rejected_all ->
      Pid ! rejected_all,
      external_choice_loop(PidList, PidParent, Renaming)
  end.

finish_external_choice(NodesFinished) ->
  case printer:print(tick_EC) of
    ok -> {finished, get_self(), NodesFinished};
    Error -> throw(Error)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Other Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rename_event(Event, [List | TailRenaming]) ->
  ShuffledList =
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])],
  rename_event(rename_event_list(Event, ShuffledList), TailRenaming);
% rename_event(Event,[_|TailRenaming]) ->
% 	rename_event(Event,TailRenaming);
rename_event(Event, []) ->
  Event.

rename_event_list(Event, [{rename, Event, Renamed} | _]) ->
  Renamed;
rename_event_list(Event, [_ | Tail]) ->
  rename_event_list(Event, Tail);
rename_event_list(Event, []) ->
  Event.

%vore que no quede una unica cria a aquesta funció
print_skip(SPAN, GraphParent, PidSkip, IsFinal) ->
  NGraphParent = printer:create_graph({skip, SPAN}, GraphParent),
  case IsFinal of
    true -> printer:print('   tick');
    false -> printer:print('   tau')
  end,
  PidSkip ! {continue_skip, NGraphParent}.

remove_temporal_graph(Pid, TemporalGraphs) ->
  case TemporalGraphs of
    {{Pid, IdAno}, Other} ->
      case printer:remove_graph_no_ided(IdAno) of
        ok -> {{}, Other};
        Error -> throw(Error)
      end;
    {Other, {Pid, IdAno}} ->
      case printer:remove_graph_no_ided(IdAno) of
        ok -> {Other, {}};
        Error -> throw(Error)
      end;
    _ ->
      TemporalGraphs
  end.

create_temporal_graph(Pid, {TGraphA, TGraphB}, Prefixing, GraphParent, PidA, PidB) ->
  case Prefixing of
    {} ->
      {TGraphA, TGraphB};
    _ ->
      IdAno = printer:create_graph_no_ided(Prefixing, GraphParent),
      case Pid of
        PidA -> {{Pid, IdAno}, TGraphB};
        PidB -> {TGraphA, {Pid, IdAno}}
      end
  end.

random_branches(PA, PB) ->
  Selected = rand:uniform(2),
  case Selected of
    1 -> {PA, PB};
    2 -> {PB, PA}
  end.

select_channels([{out, Channel} | Tail], Event) ->
  % io:format("Event: ~p\nChannel: ~p\n",[Event,Channel]),
  [Channel | select_channels(Tail, Event)];
select_channels([{'inGuard', _, ChannelsList} | Tail], Event) ->
  Selected = rand:uniform(length(ChannelsList)),
  [lists:nth(Selected, ChannelsList) | select_channels(Tail, Event)];
select_channels([{in, _} | Tail], Event) ->
  Channels = codeserver:ask_channel(Event),
  NTail =
    try
      lists:sublist(Tail, length(Channels), length(Tail))
    catch
      _:_ -> []
    end,
  Channels ++ select_channels(NTail, Event);
select_channels(_Other = [_ | _], _) ->
  % io:format("Other type of channel: ~p\n",[Other]),
  [];
select_channels([], _) ->
  [].

create_channels_string([]) ->
  "";
create_channels_string([Channel]) when is_integer(Channel) ->
  integer_to_list(Channel);
create_channels_string([Channel]) when is_atom(Channel) ->
  atom_to_list(Channel);
create_channels_string([Channel | Tail]) when is_integer(Channel) ->
  integer_to_list(Channel) ++ "." ++ create_channels_string(Tail);
create_channels_string([Channel | Tail]) when is_atom(Channel) ->
  atom_to_list(Channel) ++ "." ++ create_channels_string(Tail).

createDict([{'inGuard', Var, _} | TC], [Selected | TS], EE) when is_list(Var) ->
  [{list_to_atom(Var), Selected} | createDict(TC, TS, EE)];
createDict([{'inGuard', Var, _} | TC], [Selected | TS], EE) when is_atom(Var) ->
  [{Var, Selected} | createDict(TC, TS, EE)];
createDict([{in, Var} | TC], [Selected | TS], EE) when is_list(Var) ->
  [{list_to_atom(Var), Selected} | createDict(TC, TS, EE)];
createDict([{in, Var} | TC], [Selected | TS], EE) when is_atom(Var) ->
  [{Var, Selected} | createDict(TC, TS, EE)];
createDict([{in, _} | _], _, EE) ->
  throw(lists:flatten(io_lib:format("Detected an in-channel without defined options while executing event ~p", [EE])));
createDict([{out, _} | TC], [_ | TS], EE) ->
  createDict(TC, TS, EE);
createDict([], [], _) ->
  [].

get_self() ->
  catch self().
