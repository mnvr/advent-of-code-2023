default: example

.PHONY: example read run test watch verify o clean

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
	@n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	echo "cat $$in | runghc $$hs" && \
	cat $$in | runghc $$hs

test:
	@n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
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

verify:
	@ls -r *.hs | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      hs="$$n.hs"; \
	  cat $$in | runghc $$hs && \
	  echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" ;\
	done

o:
	@n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	mkdir -p out && \
	echo "ghc -O -outputdir out -o out/$$n $$hs" && \
	echo "cat $$in | time ./out/$$n" && \
	ghc -O -outputdir out -o out/$$n $$hs && \
	cat $$in | time -h ./out/$$n && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)"

clean:
	rm -rf out
