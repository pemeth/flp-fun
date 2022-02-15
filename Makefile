SRC:=Main
OUT:=flp21-fun

.PHONY: clean build

build: $(SRC).hs
	ghc $(SRC).hs -o $(OUT)

pack:
	zip -r flp-fun-xnemet04.zip Makefile *.hs doc/ test/

clean:
	rm -f $(OUT) *.hi *.o