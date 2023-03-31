all: read-syscall read-write-syscall slow-read-write alloc-dealloc-fread-fwrite instrument_tests

read-syscall: read-syscall.c
	musl-gcc -static read-syscall.c -o read-syscall
	
read-write-syscall: read-write-syscall.c
	musl-gcc -static read-write-syscall.c -o read-write-syscall

slow-read-write: slow-read-write.c
	musl-gcc -static slow-read-write.c -o slow-read-write
	
alloc-dealloc-fread-fwrite: alloc-dealloc-fread-fwrite.c
	musl-gcc -static alloc-dealloc-fread-fwrite.c -o alloc-dealloc-fread-fwrite

######################################################################
# Basic testing of instrumenting each program with various probes
######################################################################

define do_instrument
    cabal exec mctrace -- instrument \
	--binary=$2 \
	--output=$2.$3.inst \
	--library=../library/X86/runtime.o \
	--var-mapping=/tmp/$2.$3.inst.json \
	--persistence-file=/tmp/telemetry.bin \
	--script=$1
endef

instrument_tests:
	$(call do_instrument,../eval/single-add-probe.d,read-syscall,1)
	$(call do_instrument,../eval/single-add-probe.d,read-write-syscall,1)
	$(call do_instrument,../eval/single-add-probe.d,slow-read-write,1)

	$(call do_instrument,../eval/multiple-probe.d,read-syscall,2)
	$(call do_instrument,../eval/multiple-probe.d,read-write-syscall,2)
	$(call do_instrument,../eval/multiple-probe.d,slow-read-write,2)

	$(call do_instrument,../eval/entry-exit-probe.d,read-syscall,3)
	$(call do_instrument,../eval/entry-exit-probe.d,read-write-syscall,3)
	$(call do_instrument,../eval/entry-exit-probe.d,slow-read-write,3)

	$(call do_instrument,../eval/write-timing-probe.d,read-write-syscall,4)
	$(call do_instrument,../eval/write-timing-probe.d,slow-read-write,4)

	$(call do_instrument,../eval/fopen-calloc-fclose-probe.d,alloc-dealloc-fread-fwrite,1)

	$(call do_instrument,../eval/graph-probe.d,read-syscall,5)
	$(call do_instrument,../eval/graph-probe.d,read-write-syscall,5)
	$(call do_instrument,../eval/graph-probe.d,slow-read-write,5)

clean:
	find . -name '*.o' -delete
	rm read-syscall
	rm read-write-syscall
	rm slow-read-write
	rm alloc-dealloc-fread-fwrite
