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
	# Build the vendored patched version of Bryan Ford's pappy.
	# (We vendor it to avoid network fetches during builds.)
	$(MAKE) -C pappy/pappy pappy
Pappy/Parse.hs:
	mkdir Pappy || true
	${PAPPY} --2010 --write-files

# Canonical grammar: .pest + .pappy.rhs -> .pappy (then pappy -> .hs)
Lojban.pappy: Lojban.pest Lojban.pappy.rhs
	python3 scripts/gen_pappy.py Lojban.pest Lojban.pappy.rhs -o Lojban.pappy
Morphology.pappy: Morphology.pest Morphology.pappy.rhs
	python3 scripts/gen_pappy.py Morphology.pest Morphology.pappy.rhs -o Morphology.pappy

Lojban.hs: Lojban.pappy ${PAPPY}
	${PAPPY} ${PAPPYOPTS} Lojban.pappy
Morphology.hs: Morphology.pappy ${PAPPY}
	${PAPPY} ${PAPPYOPTS} Morphology.pappy
