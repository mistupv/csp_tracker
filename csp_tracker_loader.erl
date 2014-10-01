-module(csp_tracker_loader).
-export([compile/0, load/0]).

compile() -> 
  comp_aux( 'src/codeserver.erl' ),
  comp_aux( 'src/csp_process.erl' ),
  comp_aux( 'src/printer.erl' ),
  comp_aux( 'src/csp_parsing.erl'),
  comp_aux( 'src/csp_tracker.erl'),
  comp_aux( 'src/csp_bench.erl').

load() ->
  code:load_abs("ebin/codeserver"),
  code:load_abs("ebin/csp_process"),
  code:load_abs("ebin/printer"),
  code:load_abs("ebin/csp_parsing"),
  code:load_abs("ebin/csp_tracker"),
  code:load_abs("ebin/csp_bench"),
  ok.

comp_aux( File ) ->
  case compile:file( File, [{outdir, ebin}]) of
    {ok, _} -> ok;
    Error   -> io:format("Error compiling ~p:~n~p~n", [File, Error])
  end,
  ok.
