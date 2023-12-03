default: example

.PHONY: example read run test watch

example:
	@n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$n* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "cat $$in | runghc $$hs" && \
	cat $$in | runghc $$hs

read:
	@echo "runghc `ls -t *.hs | head -1`" && \
	runghc `ls -t *.hs | head -1`

run:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$f"; \
	hs=`ls -t *.hs | head -1`; \
	echo "cat $$in | runghc $$hs" && \
	cat $$in | runghc $$hs

test:
	@n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	ans=`ls -t answers/$$n* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "cat $$in | runghc $$hs" && \
	echo 'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"' && \
	cat $$in | runghc $$hs && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)"

watch:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$f* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "fswatch $$hs | while read f; do cat $$in | runghc $$hs; done" && \
	fswatch $$hs | while read f; do cat $$in | runghc $$hs; done
