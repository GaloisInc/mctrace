FROM ubuntu:22.04 AS base

# Variables to be updated
ARG GHCUP_BIN_DIR=/root/.ghcup/bin
ARG GHCUP_DWN_URL=https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
ARG GHC_VERSION=8.10.7


# Environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH=${PATH}:${GHCUP_BIN_DIR}


# Install basic stuff
RUN \
  apt-get update && \
  apt-get upgrade -y

RUN \
  apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg2

RUN \
  apt-get install -y \
    g++ \
    gcc \
    git \
    libc-dev \
    make

# And stuff specific for mctrace
RUN \
  apt-get install -y \
    libgmp-dev \
    zlib1g-dev \
    llvm-12-dev \
    llvm-12-runtime


# Install Ghcup and then haskell and cabal
RUN \
  mkdir -p ${GHCUP_BIN_DIR}


RUN \
	curl -sSL ${GHCUP_DWN_URL} > ${GHCUP_BIN_DIR}/ghcup && \
	chmod +x ${GHCUP_BIN_DIR}/ghcup

RUN \
  ghcup upgrade && \
  ghcup install cabal && \
  ghcup install ghc ${GHC_VERSION} -p 'x86_64-deb10-linux' && \
  ghcup set ghc ${GHC_VERSION}

RUN cabal update


# Get MCTrace
RUN git clone https://github.com/GaloisInc/mctrace.git /root/mctrace

# We need to tweak the git config so that submodule clones work over https
# before submodule fetch
WORKDIR /root/mctrace
RUN git config --global url."https://github.com/".insteadOf "git@github.com:"
RUN git config --global url."https://".insteadOf "git://"
RUN git submodule update --init

# Build mctrace
RUN ln -s cabal.project.dist cabal.project
RUN ln -s cabal.project.freeze.ghc-${GHC_VERSION} cabal.project.freeze
RUN cabal configure pkg:mctrace
RUN cabal build pkg:mctrace


# Final stage where we build a minimal image
FROM ubuntu:22.04

# Environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV SOURCE_MCTRACE_ROOT=/root/mctrace
ENV TARGET_MCTRACE_ROOT=/mctrace-bin

# Install packages
RUN \
  apt-get update && \
  apt-get upgrade -y

RUN apt-get install -y musl-tools make


# Copy the minimal amount of files we need to get this running
COPY --from=base \
    ${SOURCE_MCTRACE_ROOT}/dist-newstyle/build/x86_64-linux/ghc-8.10.7/mctrace-0.1.0.0/x/mctrace/build/mctrace/mctrace \
    ${TARGET_MCTRACE_ROOT}/mctrace

COPY --from=base /lib/x86_64-linux-gnu/libLLVM-12.so.1 /lib/x86_64-linux-gnu/libLLVM-12.so.1
COPY --from=base /lib/x86_64-linux-gnu/libedit.so.2 /lib/x86_64-linux-gnu/libedit.so.2
COPY --from=base /lib/x86_64-linux-gnu/libxml2.so.2 /lib/x86_64-linux-gnu/libxml2.so.2
COPY --from=base /lib/x86_64-linux-gnu/libbsd.so.0 /lib/x86_64-linux-gnu/libbsd.so.0
COPY --from=base /lib/x86_64-linux-gnu/libicuuc.so.70 /lib/x86_64-linux-gnu/libicuuc.so.70
COPY --from=base /lib/x86_64-linux-gnu/libmd.so.0 /lib/x86_64-linux-gnu/libmd.so.0
COPY --from=base /lib/x86_64-linux-gnu/libicudata.so.70 /lib/x86_64-linux-gnu/libicudata.so.70

ENV PATH=${PATH}:${TARGET_MCTRACE_ROOT}



