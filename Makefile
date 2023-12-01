default: example

.PHONY: example read run

example:
	@f=`find *.hs | head -1 | cut -f1 -d.`; \
	cat example/$$f | runghc $$f.hs

read:
	@runghc `find *.hs | head -1`

run:
	@f=`find *.hs | head -1 | cut -f1 -d.`; \
	cat inputs/$$f | runghc $$f.hs
