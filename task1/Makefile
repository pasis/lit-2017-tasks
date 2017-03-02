.PHONY: all c pas clean

all: c pas

c: task1c

pas: task1pas

task1c: task1.c
	gcc -Wall -Werror -g3 -lm -o task1c task1.c

task1pas: task1.pas
	fpc -g -otask1pas task1.pas

clean:
	rm task1c task1pas task1.o
