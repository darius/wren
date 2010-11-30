CFLAGS   := -m32 -g2 -Wall -W
LDFLAGS  := -m32

all: wren

clean:
	rm -f *.o wren examples.out

wren: wren.o
