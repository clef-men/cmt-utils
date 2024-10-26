.PHONY : all
all : build

.PHONY : build
build :
	@ dune build

.PHONY : install
install :
	@ dune install

.PHONY : top
top :
	@ dune utop .

.PHONY : clean
clean :
	@ dune clean
