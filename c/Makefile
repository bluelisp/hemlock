CC:=cc
CFLAGS=-std=c89 -pedantic -g -Wall -Wno-parentheses
LDFLAGS:=

all: setpty

.PHONY: clean
clean:
	rm -f setpty.o setpty

%.o: %.cpp
	$(CC) -c -o $@ $< -I. $(CFLAGS)

setpty: setpty.o
	$(CC) -o $@ $^ $(LDFLAGS)
