default: example

.PHONY: example read run

example:
	@f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t example/$$f* | head -1`; \
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
