# make dev - start application in developer environment

.PHONY: deps compile clean dev test dialyzer

ERL?=erl
REBAR?=./rebar

deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	$(REBAR) compile skip_deps=true

clean:
	rm -rf deps
	rm -rf ebin

test: compile
	$(REBAR) eunit skip_deps=true
