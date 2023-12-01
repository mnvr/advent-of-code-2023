default: example

.PHONY: example read run test

example:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$f* | head -1`; \
	echo "cat $$in | runghc `ls -t *.hs | head -1`" && \
	cat $$in | runghc `ls -t *.hs | head -1`

read:
	@echo "runghc `ls -t *.hs | head -1`" && \
	runghc `ls -t *.hs | head -1`

run:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$f"; \
	echo "cat $$in | runghc `ls -t *.hs | head -1`" && \
	cat $$in | runghc `ls -t *.hs | head -1`

test:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$f"; \
	ans=`ls -t answers/$$f* | head -1`; \
	echo "cat $$in | runghc `ls -t *.hs | head -1`" && \
	echo "cat $$ans" && \
	cat $$in | runghc `ls -t *.hs | head -1` && \
	cat $$ans; echo
