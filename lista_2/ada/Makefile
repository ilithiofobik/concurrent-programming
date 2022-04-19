# Makefile

all: 
	gprbuild -p

prove:
	gnatprove -P find.gpr -j0 

clean:
	gprclean
	gnatprove -P find.gpr --clean

