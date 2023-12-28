default:
	cat ../../examples/17 | swift run

.PHONY: default timings

# 1. Build with optimizations,
# 2. Run once to warm up disk cache
# 3. Run again, this time showing timings
time:
	@out=`swift build --show-bin-path -c release -Xswiftc -O` && \
	swift build -c release -Xswiftc -O >/dev/null && \
	cat ../../inputs/17 | $$out/17 >/dev/null && \
	cat ../../inputs/17 | /usr/bin/time $$out/17
