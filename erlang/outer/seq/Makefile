all: main.beam
	erl -noshell -s main main -s init stop < main.in > main.out
	diff main.out main.gold
	echo "10" | erl -noshell -s main main is_bench -s init stop 


main: main.beam
main.beam: main.erl
	erl -compile main
