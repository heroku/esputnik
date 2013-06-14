REBAR := $(shell which rebar)

default:
	$(REBAR) get-deps compile

test: default
	$(REBAR) ct skip_deps=true

console: default
	erl \
	-pa ebin deps/*/ebin -env $ERL_LIBS deps -s esputnik_app start
