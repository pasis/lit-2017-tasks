.PHONY: all c clean

all: c

c: task2

task2: task2.c ../task1/task1.c
	gcc -Wall -Werror -g3 -lm `sdl2-config --cflags --libs` -o task2 task2.c

clean:
	rm task2
