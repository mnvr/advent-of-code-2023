default: example

.PHONY: example read run

example:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t example/$$f* | head -1`; \
	cat $$in | runghc $$f.hs

read:
	@runghc `ls -t *.hs | head -1`

run:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	cat inputs/$$f | runghc $$f.hs
