.PHONY: translator parseCspForPl
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	EXEC_MODE := linux
else
	EXEC_MODE := docker
endif

all: $(EXEC_MODE)
	erlc csp_tracker_loader.erl
	erl -run csp_tracker_loader compile -noshell -s erlang halt

linux: translator parseCspForPl

docker: translator parseCspForPl
	@echo "There are no binaries for your platform, using docker."
	@echo "Docker must be active when executing the program."

translator: bin_linux/translator
	ln -sf $< $@

parseCspForPl: bin_linux/parseCspForPl
	ln -sf $< $@

clean:
	rm -f ebin/*.beam
	rm -f *.beam
	rm -f translator parseCspForPl
