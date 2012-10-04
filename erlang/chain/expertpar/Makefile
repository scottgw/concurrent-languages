all: main.beam
	erl -noshell -s main main -s init stop < main.in

main: main.beam
main.beam: main.erl randmat.erl thresh.erl winnow.erl outer.erl product.erl
	erl -compile $^ 
