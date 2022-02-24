# Makefile for the flp-fun project.
#
# Project:    plg-2-nka
# Year:       2021/2022
# Author:     Patrik Nemeth
# Xlogin:     xnemet04
# Email:      xnemet04@stud.fit.vutbr.cz

SRC:=Main
OUT:=flp21-fun

.PHONY: clean build test

build: $(SRC).hs
	ghc $(SRC).hs -o $(OUT)

test:
	@./run_tests.sh

pack:
	zip -r flp-fun-xnemet04.zip Makefile *.hs README.md test/ run_tests.sh

clean:
	rm -f $(OUT) *.hi *.o