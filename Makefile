all: compile

compile: clean
	erlc -I include/ -o ebin/ src/bitplus.erl

test: clean
	erlc -DTEST -I include/ -I test/ -o ebin/ src/bitplus.erl
	erl -pa ebin/ -noshell -s bitplus test -s init stop

clean:
	rm -rfv ebin/*.beam
