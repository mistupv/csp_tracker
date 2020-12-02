.PHONY: translator parseCspForPl
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	EXEC_MODE := linux
else
	EXEC_MODE := docker
endif
ERLC_FLAGS := +warn_all
ERLS := $(wildcard src/*.erl)
BEAMS := $(patsubst src/%.erl,ebin/%.beam,$(ERLS))

all: $(EXEC_MODE) $(BEAMS)

ebin/%.beam: src/%.erl
	erlc -o ebin ${ERLC_FLAGS} $<

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
	rm -f translator parseCspForPl
