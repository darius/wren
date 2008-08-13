CFLAGS   := -g2 -Wall -W

all: wren

clean:
	rm -f *.o wren examples.out

wren: wren.o
