all:  diff

md: grog.md sylvain.md
# diff: grog.diff sylvain.diff marcus.diff
formats: charactersheet.pdf charactersheet.html

marcus.md:
sylvain.md:
grog.md:

.force:

O=Ontology/resources.ttl Ontology/arm.ttl

%.md: Test/%.ttl .force $O
	cabal run cli -- -c $< -s Test/diedne.ttl -o $@ -O $*.triples

%: Test/%.ttl .force $O
	cabal run cli -- -c $< -s Test/diedne.ttl -D $@

test: Test/marcus.ttl 
	cabal run cli -- -s Test/diedne.ttl 
saga: Saga/verditius.ttl 
	cabal run cli -- -s Saga/newsaga.ttl 
prof: Test/marcus.ttl 
	cabal run cli --enable-profiling -- -s Test/diedne.ttl +RTS -p
	# --profiling-detail=exported-functions 

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<

Ontology/%.ttl: .force
	( cd Ontology ; $(MAKE) $*.ttl )

diff: test
	-sh diff.sh

wc:
	find src -name "*.hs" | xargs wc


Saga/verditius.ttl.md: Saga/newsaga.ttl Saga/verditius.ttl
	cabal run cli -- -s $<
Saga/tremere.ttl.md: Saga/newsaga.ttl Saga/tremere.ttl
	cabal run cli -- -s $<

valentin.md: Saga/verditius-background.md Saga/verditius.ttl.md Saga/verditius.ttl-chargen.md
	cat $^ > $@
torbjorn.md: Saga/tremere-background.md Saga/tremere.ttl.md Saga/tremere.ttl-chargen.md
	cat $^ > $@

%.html: %.md
	pandoc -o $@ $<

%.pdf: %.md
	pandoc -o $@ $<
