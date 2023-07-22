MAIN_BIN=tersmu

PAPPY=pappy/pappy/pappy
PAPPYOPTS=--2010 -e --monad

.PHONY: warn install build
warn:
	@echo "Use \"make install\" to download and compile dependencies and install tersmu"
install: *.hs Lojban.hs Morphology.hs Pappy/Parse.hs
	cabal update && cabal install

build: *.hs Lojban.hs Morphology.hs Pappy/Parse.hs
	cabal build

${PAPPY}:
	# get my patched version of Chris Done's version of Bryan Ford's pappy
	darcs clone http://mbays.freeshell.org/pappy
	cd pappy/pappy && make
Pappy/Parse.hs:
	mkdir Pappy || true
	${PAPPY} --2010 --write-files
Lojban.hs: Lojban.pappy ${PAPPY}
	${PAPPY} ${PAPPYOPTS} Lojban.pappy
Morphology.hs: Morphology.pappy ${PAPPY}
	${PAPPY} ${PAPPYOPTS} Morphology.pappy
