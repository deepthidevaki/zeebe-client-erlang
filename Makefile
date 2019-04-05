REBAR = rebar3
.PHONY: rel test

all: compile

compile:
	$(REBAR) grpc gen
	$(REBAR) compile

tests: compile	
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -dir test/

clean:
	$(REBAR) clean

distclean: clean relclean cleantests
	$(REBAR) clean --all

cleantests:
	rm -f test/*.beam

rel:
	$(REBAR) release

relclean:
	rm -rf _build/default/rel

check: distclean test dialyzer

dialyzer:
	${REBAR} dialyzer
