.PHONY: clean
clean:
	dune clean

.PHONY: start
start:
	dune exec ./week_focus.exe

.PHONY: build
build:
	dune build ./week_focus.exe
