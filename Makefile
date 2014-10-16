all: tty clx

install: 
	-cp hemlock /usr/local/bin
	-cp hemlock-tty /usr/local/bin

tty:
	./build.sh tty
	mv hemlock hemlock-tty

clx:
	./build.sh 
.PHONY: hemlock install clx
