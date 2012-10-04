all: main.beam
	erl -noshell -s main main -s init stop < main.in > main.out
	diff main.out main.gold

main.beam: main.erl
	erl -compile main
