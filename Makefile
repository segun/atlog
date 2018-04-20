all: 
	rebar clean get-deps compile

deps:
	rebar clean get-deps

build:
	rebar clean compile

run: build
	#cp libs/* ebin/
	@erl -setcookie secret -sname atlog -pa ebin/ -pa deps/*/ebin/ -s atlog start

clean:
	rebar clean

test: build
	@mkdir ebin/tests
	@erlc -pa 'ebin/' -pa 'deps/erlog/ebin' -pa 'deps/erlp3tags/ebin' -pa 'deps/emysql/ebin' -o 'ebin/tests' tests/*.erl
	@erl -pa 'ebin/' -pa 'deps/erlog/ebin' -pa 'deps/erlp3tags/ebin' -pa 'deps/emysql/ebin' -pa 'ebin/tests' -noshell -s mp3booster_tests tests -s init stop
