FROM ubuntu:22.04 AS base

# Environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install basic stuff
RUN \
  apt-get update && \
  apt-get upgrade -y

RUN \
  apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg2 \
    git \
    sudo

# Get MCTrace
RUN git clone https://github.com/GaloisInc/mctrace.git /root/mctrace -b feature/powerpc-support

WORKDIR /root/mctrace

# We need to tweak the git config so that submodule clones work over https
# before submodule fetch
RUN git config --global url."https://github.com/".insteadOf "git@github.com:"
RUN git config --global url."https://".insteadOf "git://"

ENV SKIP_DOCKER=1
RUN ./dev_setup.sh

ENV PATH=${PATH}:/root/.ghcup/bin
RUN cabal update

# Build mctrace
RUN ./build.sh

# Build examples for X86
RUN make -C mctrace/tests/full
# Build examples for PPC
RUN make -C mctrace/tests/full ARCH=PPC

# Final stage where we build a minimal image
FROM ubuntu:22.04

# Environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV SOURCE_MCTRACE_ROOT=/root/mctrace
ENV TARGET_MCTRACE_BIN=/mctrace-bin
ENV TARGET_MCTRACE_ROOT=/mctrace-test

# Install packages
RUN \
  apt-get update && \
  apt-get upgrade -y

RUN apt-get install -y musl-tools make python3 qemu-user qemu-user-binfmt vim less

# Copy the minimal amount of files we need to get mctrace running
COPY --from=base \
    ${SOURCE_MCTRACE_ROOT}/dist-newstyle/build/x86_64-linux/ghc-8.10.7/mctrace-0.1.0.0/x/mctrace/build/mctrace/mctrace \
    ${TARGET_MCTRACE_BIN}/mctrace

COPY --from=base /lib/x86_64-linux-gnu/libLLVM-12.so.1 /lib/x86_64-linux-gnu/libLLVM-12.so.1
COPY --from=base /lib/x86_64-linux-gnu/libedit.so.2 /lib/x86_64-linux-gnu/libedit.so.2
COPY --from=base /lib/x86_64-linux-gnu/libxml2.so.2 /lib/x86_64-linux-gnu/libxml2.so.2
COPY --from=base /lib/x86_64-linux-gnu/libbsd.so.0 /lib/x86_64-linux-gnu/libbsd.so.0
COPY --from=base /lib/x86_64-linux-gnu/libicuuc.so.70 /lib/x86_64-linux-gnu/libicuuc.so.70
COPY --from=base /lib/x86_64-linux-gnu/libmd.so.0 /lib/x86_64-linux-gnu/libmd.so.0
COPY --from=base /lib/x86_64-linux-gnu/libicudata.so.70 /lib/x86_64-linux-gnu/libicudata.so.70

# Copy example probes and binaries
COPY --from=base ${SOURCE_MCTRACE_ROOT}/mctrace/tests /${TARGET_MCTRACE_ROOT}/examples

# Copy documentation
COPY --from=base ${SOURCE_MCTRACE_ROOT}/docs/README-release.md /${TARGET_MCTRACE_ROOT}/README.md

# Copy extractor script
COPY --from=base ${SOURCE_MCTRACE_ROOT}/scripts/extractor.py ${TARGET_MCTRACE_BIN}

# Copy powerpc musl-gcc compiler
COPY --from=base ${SOURCE_MCTRACE_ROOT}/musl-gcc/output ${TARGET_MCTRACE_BIN}/ppc-musl-gcc

# Adjust paths to bring useful tools on to the path
ENV PATH=${PATH}:${TARGET_MCTRACE_BIN}:${TARGET_MCTRACE_BIN}/ppc-musl-gcc/bin
ENV DOCKER=1

