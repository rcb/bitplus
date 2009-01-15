all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam
