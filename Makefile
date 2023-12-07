default: example

.PHONY: example read run test watch verify o clean

# For printing colored strings, we use escape sequences
#
#     echo -e "\033[2;80mSome text\033[0m"
#
# First one sets the color, second one resets it. The money bit is "[2;80m". The
# 2 here means decreased intensity (other interesting values are 0 for normal, 1
# for bold). The 80m is the color - it is an arbitrary value I picked up for a
# gray that should work on both light and dark colored terminals (there
# currently does not seem to do a mode aware lower intensity "gray" output).

example:
	@dim='\033[2;80m'; reset='\033[0m'; \
	n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$n* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "$$dim""cat $$in | runghc $$hs""$$reset" && \
	cat $$in | runghc $$hs

read:
	@echo "runghc `ls -t *.hs | head -1`" && \
	runghc `ls -t *.hs | head -1`

run:
	@dim='\033[2;80m'; reset='\033[0m'; \
	n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	echo "$$dim""cat $$in | runghc $$hs""$$reset" && \
	cat $$in | runghc $$hs

test:
	@dim='\033[2;80m'; reset='\033[0m'; \
	n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	echo "$$dim""cat $$in | runghc $$hs""$$reset" && \
	echo "$$dim"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$$reset" && \
	cat $$in | runghc $$hs && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)"

watch:
	@dim='\033[2;80m'; reset='\033[0m'; \
	f=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in=`ls -t examples/$$f* | head -1`; \
	hs=`ls -t *.hs | head -1`; \
	echo "$$dim""fswatch $$hs | while read f; do cat $$in | runghc $$hs; done""$$reset" && \
	fswatch $$hs | while read f; do cat $$in | runghc $$hs; done

verify:
	@ls -r *.hs | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      hs="$$n.hs"; \
	  cat $$in | runghc $$hs && \
	  echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" ;\
	done

o:
	@dim='\033[2;80m'; reset='\033[0m'; \
	n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	mkdir -p out && \
	echo "$$dim""ghc -O -outputdir out -o out/$$n $$hs""$$reset" && \
	echo "$$dim""cat $$in | time ./out/$$n""$$reset" && \
	ghc -O -outputdir out -o out/$$n $$hs && \
	cat $$in | time -h ./out/$$n && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)"

o2:
	@dim='\033[2;80m'; reset='\033[0m'; \
	n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	mkdir -p out && \
	echo "$$dim""ghc -O2 -outputdir out -o out/$$n $$hs""$$reset" && \
	echo "$$dim""cat $$in | time ./out/$$n""$$reset" && \
	ghc -O -outputdir out -o out/$$n $$hs && \
	cat $$in | time -h ./out/$$n && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)"

clean:
	rm -rf out
