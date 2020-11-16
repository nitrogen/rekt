all:
	./rebar3 compile

publish:
	./rebar3 upgrade
	./rebar3 hex publish
