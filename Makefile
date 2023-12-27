default: example

.PHONY: example test o o2 verify run-all min hs swift clean

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

# Clear the current line
tclear = \033[2K
# Go to the start of the current line
tstart = \033[0G

latest = n=`ls -t *.hs | head -1 | cut -f1 -d.`; \
	eg=`ls -t examples/$$n* | head -1`; \
	in="inputs/$$n"; \
	f=`ls -t *.hs | head -1`; \
	mkdir -p out

latestSwift = n=`ls -t *.swift | head -1 | cut -f1 -d.`; \
	eg=`ls -t examples/$$n* | head -1`; \
	in="inputs/$$n"; \
	f=`ls -t *.swift | head -1`; \
	mkdir -p out

# The output of Haskell programs is "(p1,p2)"
check = echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" > out/expected && \
	if diff --color=always --unified out/expected out/actual; then true; else \
	echo "$(tbold)""ERROR: The program's output did not match the expected output""$(treset)" && exit 1; fi && \
	echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)""$(tgreen)"' *'"$(treset)"

# The output of Swift programs is "p1 p2"
checkSwift = echo "`cat answers/$$n-a` `cat answers/$$n-b`" > out/expected && \
	if diff --color=always --unified out/expected out/actual; then true; else \
	echo "$(tbold)""ERROR: The program's output did not match the expected output""$(treset)" && exit 1; fi && \
	echo "`cat answers/$$n-a` `cat answers/$$n-b`""$(tgreen)"' *'"$(treset)"

stats_set = ts=`cat out/time | grep real | cut -d ' ' -f2`; \
	ch=`sed '/^-- /d' $$f | wc -m | tr -d ' '`; \
	nl=`sed '/^-- /d' $$f | wc -l | tr -d ' '`; \

stats = $(stats_set) \
	cs=`test $$ch -lt 999 && echo "$$ch chars "`; \
	echo "$(tdim)"$$f $$cs$$nl lines"$(treset)" $$ts s

example:
	@ft=`ls -t *.swift *.hs | head -1 | tr '.' '\n' | tail -1`; \
	if test "$$ft" == "swift"; then $(exampleSwift); else $(exampleHaskell); fi

exampleHaskell = $(latest) && \
	echo "$(tdim)""cat $$eg | runghc $$f""$(treset)" && \
	cat $$eg | runghc $$f

exampleSwift = $(latestSwift) && \
	echo "$(tdim)""cat $$eg | swift $$f""$(treset)" && \
	cat $$eg | swift $$f

hs:
	@$(exampleHaskell)

swift:
	@$(exampleSwift)

test:
	@ft=`ls -t *.swift *.hs | head -1 | tr '.' '\n' | tail -1`; \
	if test "$$ft" == "swift"; then $(testSwift); else $(testHaskell); fi

testHaskell = $(latest) && \
	echo "$(tdim)""cat $$in | runghc $$f""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	cat $$in | command time -p -o out/time runghc $$f | tee out/actual && \
	$(check) && \
	$(stats)

testSwift = $(latestSwift) && \
	echo "$(tdim)""cat $$in | swift $$f""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	cat $$in | command time -p -o out/time swift $$f | tee out/actual && \
	$(checkSwift) && \
	$(stats)

o:
	@ft=`ls -t *.swift *.hs | head -1 | tr '.' '\n' | tail -1`; \
	if test "$$ft" == "swift"; then $(oSwift); else $(oHaskell); fi

oHaskell = $(latest) && \
	echo "$(tdim)""ghc -O -outputdir out -o out/$$n $$f""$(treset)" && \
	echo "$(tdim)""cat $$in | ./out/$$n""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	ghc -O -outputdir out -o out/$$n $$f && \
	cat $$in | command time -p -o out/time ./out/$$n | tee out/actual && \
	$(check) && \
	$(stats)

oSwift = $(latestSwift) && \
	echo "$(tdim)""swiftc -O -o out/$$n.sw $$f""$(treset)" && \
	echo "$(tdim)""cat $$in | ./out/$$n.sw""$(treset)" && \
	echo "$(tdim)"'echo "`cat answers/'$$n'-a` `cat answers/'$$n'-b`"'"$(treset)" && \
	swiftc -O -o out/$$n.sw $$f && \
	cat $$in | command time -p -o out/time ./out/$$n.sw | tee out/actual && \
	$(checkSwift) && \
	$(stats)

o2:
	@$(latest) && \
	echo "$(tdim)""ghc -O2 -outputdir out -o out/$$n $$f""$(treset)" && \
	echo "$(tdim)""cat $$in | ./out/$$n""$(treset)" && \
	echo "$(tdim)"'echo "(`cat answers/'$$n'-a`,`cat answers/'$$n'-b`)"'"$(treset)" && \
	ghc -O2 -outputdir out -o out/$$n $$f && \
	cat $$in | command time -p -o out/time ./out/$$n | tee out/actual && \
	$(check) && \
	$(stats)

awk_stats = awk 'BEGIN { a=9999 } { i=$$1; if(i<a)a=i; if(i>b)b=i; c+=1; s+=i; } END { printf " min %d avg %d max %d sum %d\n", a, s/c, b, s }'
awk_stats_f = awk 'BEGIN { a=9999 } { i=$$1; if(i<a)a=i; if(i>b)b=i; c+=1; s+=i; } END { printf " min %1.2f avg %1.2f max %1.2f sum %1.2f\n", a, s/c, b, s }'

verify:
	@export pi=1; \
	pprefix="Precompiling..." ; \
	pc='▓▒░'; \
	pc="$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc$$pc"; \
	mkdir -p out && \
	ls -r *.hs | grep -v wip | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      f="$$n.hs"; \
	  pghc="ghc -O2 -outputdir out -o out/$$n $$f" \
	  pprog=`echo $$pc | cut -c $$pi`; \
	  export pi=`expr 1 + $$pi`; \
	  psuffix="   $$pprog"; \
      printf "$(tclear)$(tstart)$$pprefix  $(tdim)$$pghc$(treset)$$psuffix" && \
      ghc -O2 -outputdir out -o out/$$n $$f >/dev/null 2>&1 ; \
	done && \
	echo "$(tclear)$(tstart)$$pprefix done" && \
	export pi=1; \
	pprefix="Warming disk caches..." ; \
	ls -r *.hs | grep -v wip | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      f="$$n.hs"; \
	  pf="$$f" \
	  pprog=`echo $$pc | cut -c $$pi`; \
	  export pi=`expr 1 + $$pi`; \
	  psuffix="   $$pprog"; \
      printf "$(tclear)$(tstart)$$pprefix  $(tdim)$$pf$(treset)$$psuffix" && \
      cat $$in | ./out/$$n >/dev/null 2>&1 ; \
	done && \
	echo "$(tclear)$(tstart)$$pprefix done" && \
	rm -f "out/stats" && \
	ls -r *.hs | grep -v wip | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
	  f="$$n.hs"; \
	  output=`cat $$in | command time -p -o out/time ./out/$$n | tee out/actual | tr '\n' ' '` && \
	  echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" > out/expected && \
	  if diff --color=always --unified out/expected out/actual; then true; else \
	  echo "$(tbold)""ERROR: The program's output did not match the expected output""$(treset)" && exit 1; fi && \
	  $(stats_set) \
	  cs=`if test $$ch -lt 999; then echo "$$ch chars"; else echo ">1k chars"; fi`; \
	  echo "$$f $$cs $$nl lines $$ts s\t$(tgreen)$$output$(treset)" && \
	  echo "$$ch $$nl $$ts" >> out/stats ; \
	done && \
	printf "ch" && cat out/stats | cut -d ' ' -f 1 | $(awk_stats) && \
	printf "nl" && cat out/stats | cut -d ' ' -f 2 | $(awk_stats) && \
	printf "$(tbold)ts" && cat out/stats | cut -d ' ' -f 3 | $(awk_stats_f) && printf "$(treset)"

run-all:
	@ls -r *.hs | cut -f1 -d. | uniq | while read n; do \
	  in="inputs/$$n"; \
      for hs in $$n*.hs; do \
      echo "$(tdim)""cat $$in | runghc $$f""$(treset)" && \
	  cat $$in | runghc $$f && \
	  echo "(`cat answers/$$n-a`,`cat answers/$$n-b`)" ; \
	  done; \
	done

min:
	@f=`ls -t *.hs | grep -v '.min.hs' | head -1`; mkdir -p out && \
	echo "$(tdim)""./tools/min.sh $$f""$(treset)" && \
	./tools/min.sh $$f

clean:
	rm -rf out
