default: example

.PHONY: example read run test watch

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

watch:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$f* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "fswatch $$hs | while read f; do cat $$in | runghc $$hs; done" && \
	fswatch $$hs | while read f; do cat $$in | runghc $$hs; done
