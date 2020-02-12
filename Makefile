.PHONY: translator parseCspForPl
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	BIN_DIR := bin_linux
endif
ifeq ($(UNAME_S),Darwin)
	BIN_DIR := bin_macos
endif

all: translator parseCspForPl
	erlc csp_tracker_loader.erl
	erl -run csp_tracker_loader compile -noshell -s erlang halt

translator:
	ln -sf $(BIN_DIR)/$@ ./$@

parseCspForPl:
	ln -sf $(BIN_DIR)/$@ ./$@

clean:
	rm -f ebin/*.beam
	rm -f *.beam
