HC=ghc
HCOPTS=-fno-warn-tabs

all: pappy

pappy: *.hs RawFiles.hs
	$(HC) $(HCOPTS) --make Main.hs -o $@

clean:
	rm -f -- *.hi *.o pappy Pappy/*.hi Pappy/*.o examples/*.hi examples/*.o examples/*.hs

RAWFILES = PappyPos.hs PappyParse.hs PappyBasic.hs

RawFiles.hs: $(RAWFILES)
	perl op_raw.prl RawFiles $(RAWFILES) > $@

examples: pappy examples/Arith   examples/Java.hs

examples/%.hs: examples/%.pappy pappy
	./pappy $< -o $@

examples/%.M.hs: examples/%.pappy pappy
	./pappy --monad $< -o $@

examples/%.E.hs: examples/%.pappy pappy $(RAWFILES)
	./pappy -e $< -o $@

examples/%.ME.hs: examples/%.pappy pappy $(RAWFILES)
	./pappy --monad -e $< -o $@

examples/%: examples/%.hs
	$(HC) $(HCOPTS) --make $< -o $@

install: pappy
	cp $< /usr/local/bin

tests: examples/Arith examples/Arith.M examples/Arith.E examples/Arith.ME
