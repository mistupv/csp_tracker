all:
	erlc csp_tracker_loader.erl
	erl -run csp_tracker_loader compile -noshell -s erlang halt

clean:
	rm ebin/*.beam
