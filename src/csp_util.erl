%% Copyright (c) 2019-2020 Salvador Tamarit. All rights reserved.
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%
-module(csp_util).

%% API
-export([send_message/2,stop/1,stop_and_wait/1,register_once/2]).

register_once(Name, SpawnFunc) ->
  case lists:member(Name, registered()) of
    true -> ok;
    false -> register(Name, spawn(SpawnFunc))
  end.

named_pid(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw({process_not_found, Name});
    Pid -> case is_process_alive(Pid) of
             true -> Pid;
             false -> throw({process_not_alive, Name, Pid})
           end
  end.

send_message(ProcessName, Message) when is_atom(ProcessName) ->
  named_pid(ProcessName) ! Message.

stop(ProcessName) when is_atom(ProcessName) ->
  catch send_message(ProcessName, stop).

stop_and_wait(ProcessName) when is_atom(ProcessName) ->
  try
    send_message(ProcessName, {stop, self()}),
    receive stopped -> ok end
  catch
    _:_ -> ok
  end.
