default: example

.PHONY: example

example:
	@f=`find *.hs | head -1 | cut -f1 -d.`; \
	cat example/$$f | runghc $$f.hs
