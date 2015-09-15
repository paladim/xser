REBAR=$(shell which rebar || echo ./rebar)

.PHONY: all compile test

all: compile

compile:
	$(REBAR) compile

eunit:
	$(REBAR) eunit

test: eunit
