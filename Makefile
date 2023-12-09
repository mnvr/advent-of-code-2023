default: example

.PHONY: example test o o2 verify min clean

# For printing colored strings, we use escape sequences
#
#     echo -e "\033[2;80mSome text\033[0m"
#
# First one sets the color, second one resets it. The money bit is "[2;80m". The
# 2 here means decreased intensity (other interesting values are 0 for normal, 1
# for bold). The 80m is the color - it is an arbitrary value I picked up for a
# gray that should work on both light and dark colored terminals (there
# currently does not seem to do a mode aware lower intensity "gray" output).

tdim = \033[2;80m
treset = \033[0m
tbold = \033[1;1m
tgreen = \033[0;32m

latest = n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	eg=`ls -t examples/$$n* | head -1`; \
	in="inputs/$$n"; \
	hs=`ls -t *.hs | head -1`; \
	mkdir -p out

check = echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" > out/expected && \
	if diff --color=always --unified out/expected out/actual; then true; else \
	echo "$(tbold)""ERROR: The program's output did not match the expected output""$(treset)" && exit 1; fi && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)""$(tgreen)"' *'"$(treset)"

stats = ts=`cat out/time | grep real | cut -d ' ' -f2`; \
	ch=`wc -m < $$hs | tr -d ' '`; \
	nl=`wc -l < $$hs | tr -d ' '`; \
	cs=`test $$ch -lt 999 && echo "$$ch chars "`; \
	echo "$(tdim)"$$hs $$cs$$nl lines"$(treset)" $$ts s

example:
	@$(latest) && \
	echo "$(tdim)""cat $$eg | runghc $$hs""$(treset)" && \
	cat $$eg | runghc $$hs

test:
	@$(latest) && \
	echo "$(tdim)""cat $$in | runghc $$hs""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	cat $$in | command time -p -o out/time runghc $$hs | tee out/actual && \
	$(check) && \
	$(stats)

o:
	@$(latest) && \
	echo "$(tdim)""ghc -O -outputdir out -o out/$$n $$hs""$(treset)" && \
	echo "$(tdim)""cat $$in | ./out/$$n""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	ghc -O -outputdir out -o out/$$n $$hs && \
	cat $$in | command time -p -o out/time ./out/$$n | tee out/actual && \
	$(check) && \
	$(stats)

o2:
	@$(latest) && \
	echo "$(tdim)""ghc -O2 -outputdir out -o out/$$n $$hs""$(treset)" && \
	echo "$(tdim)""cat $$in | ./out/$$n""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	ghc -O2 -outputdir out -o out/$$n $$hs && \
	cat $$in | command time -p -o out/time ./out/$$n | tee out/actual && \
	$(check) && \
	$(stats)

verify:
	@ls -r *.hs | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      hs="$$n.hs"; \
	  cat $$in | runghc $$hs && \
	  echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" ;\
	done

min:
	@hs=`ls -t *.hs | grep -v '.min.hs' | head -1`; \
	echo "$(tdim)""./tools/min.sh $$hs""$(treset)" && \
	./tools/min.sh $$hs

clean:
	rm -rf out
