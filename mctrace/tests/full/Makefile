
ARCH ?= X86

PROGRAMS = read-syscall-$(ARCH) \
	   read-write-syscall-$(ARCH) \
	   slow-read-write-$(ARCH) \
	   alloc-dealloc-fread-fwrite-$(ARCH)

ifeq ($(DOCKER),1)
    # In Docker, mctrace is in the path (See Dockerfile)
    MCTRACE = mctrace
else
    MCTRACE = cabal exec mctrace
endif

ifeq ($(ARCH),PPC)
    ifeq ($(DOCKER),1)
        # In Docker, the cross compiler is in the path (See Dockerfile)
        CC = powerpc-linux-muslsf-gcc
    else
        CC = ../../../musl-gcc/output/bin/powerpc-linux-muslsf-gcc
    endif
else ifeq ($(ARCH),ARM)
    ifeq ($(DOCKER),1)
        # In Docker, the cross compiler is in the path (See Dockerfile)
        CC = arm-linux-musleabi-gcc
    else
        CC = ../../../musl-gcc-arm/output/bin/arm-linux-musleabi-gcc
    endif
else ifeq ($(ARCH),X86)
    CC = musl-gcc
else
    $(error $(ARCH) - unknown architecture)
endif

all: platform_impl $(PROGRAMS) instrument_tests instrument_binaries

platform_impl:
	$(CC) -c -I../library ../library/$(ARCH)/platform_impl.c -o ../library/$(ARCH)/platform_impl.o

read-syscall-$(ARCH): read-syscall.c
	$(CC) -static read-syscall.c -o read-syscall-$(ARCH)
	
read-write-syscall-$(ARCH): read-write-syscall.c
	$(CC) -static read-write-syscall.c -o read-write-syscall-$(ARCH)

slow-read-write-$(ARCH): slow-read-write.c
	$(CC) -static slow-read-write.c -o slow-read-write-$(ARCH)
	
alloc-dealloc-fread-fwrite-$(ARCH): alloc-dealloc-fread-fwrite.c
	$(CC) -static alloc-dealloc-fread-fwrite.c -o alloc-dealloc-fread-fwrite-$(ARCH)

######################################################################
# Basic testing of instrumenting each program with various probes
######################################################################

define do_instrument
    $(MCTRACE) -- instrument \
	--binary=$2-$(ARCH) \
	--output=$2-$(ARCH).$3.inst \
	--library=../library/$(ARCH)/platform_impl.o \
	--var-mapping=$2-$(ARCH).$3.inst.json \
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

######################################################################
# Additional testing with prebuilt binaries and associated probes.
######################################################################

define do_instrument_binary
    $(MCTRACE) -- instrument \
	--binary=../binaries/$(ARCH)/$2 \
	--output=$2-$(ARCH).$3.inst \
	--library=../library/$(ARCH)/platform_impl.o \
	--var-mapping=/tmp/$2-$(ARCH).$3.inst.json \
	--script=$1
endef

# Prebuilt binaries are generally real programs and it may not be possible
# that every binary has a counterpart on each supported architecture. Instead
# we define binary/probe combinations for each architecture separately. The
# ARCH variable determines the exact set that gets instrumented for any 
# particular run.


instrument_binaries_PPC:
	$(call do_instrument_binary,../eval/cat.probe.d,cat,1)
	$(call do_instrument_binary,../eval/sha256sum.probe.d,sha256sum,1)

instrument_binaries_X86:
	$(call do_instrument_binary,../eval/cat.probe.d,cat,1)
	$(call do_instrument_binary,../eval/sha256sum.probe.d,sha256sum,1)

instrument_binaries: $(if $(filter PPC,$(ARCH)),instrument_binaries_PPC,instrument_binaries_X86)

clean:
	find . -name '*.o' -delete
	rm -f *.inst *-X86 *-PPC *-ARM ../library/$(ARCH)/*.o *.json
