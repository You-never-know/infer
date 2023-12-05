# Author: Dominik Harmim <iharmim@fit.vut.cz>

FROM debian:bullseye-slim

LABEL maintainer "Dominik Harmim <iharmim@fit.vut.cz>"

RUN apt-get update && \
    mkdir -p /usr/share/man/man1 && \
    apt-get install -y --no-install-recommends \
      autoconf \
      automake \
      bubblewrap \
      bzip2 \
      cmake \
      curl \
      gcc \
      g++ \
      git \
      libc6-dev \
      libgmp-dev \
      libmpfr-dev \
      libsqlite3-dev \
      sqlite3 \
      make \
      opam \
      openjdk-11-jdk-headless \
      patch \
      patchelf \
      pkg-config \
      python3 \
      python3-distutils \
      unzip \
      xz-utils \
      zlib1g-dev \
      ninja-build \
      vim \
      man \
      jq \
      parallel \
      locales \
      rsync \
      shellcheck && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# set an UTF-8 locale
RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG en_GB.UTF-8
ENV LANGUAGE en_GB:en
ENV LC_ALL en_GB.UTF-8

# download a Bash configuration
RUN git clone https://github.com/harmim/debian-bash-config.git /config && \
    cp -R /config/. /root && \
    rm -rf /config

# Disable sandboxing of Opam. Without this, Opam fails to compile OCaml for
# some reason. We don't need sandboxing inside a Docker container anyway.
RUN opam init --reinit --bare --disable-sandboxing -y --auto-setup

# download the latest Atomer (the 'atomicity-sets' branch in Infer)
RUN git clone https://github.com/harmim/infer.git /infer -b atomicity-sets

# Build Opam deps first, then Clang, then Infer. This way, if any step fails
# we don't lose the significant amount of work done in the previous steps.
RUN cd /infer && ./build-infer.sh -y --only-setup-opam
RUN cd /infer && \
    eval $(opam env) && \
    ./autogen.sh && \
    ./configure && \
    ./facebook-clang-plugins/clang/src/prepare_clang_src.sh && \
    ./facebook-clang-plugins/clang/setup.sh --ninja --sequential-link
RUN cd /infer && make -j

# install Infer
RUN cd /infer && make install -j BUILD_MODE=opt
ENV INFER_HOME /infer/infer
ENV PATH "${INFER_HOME}/bin:${PATH}"
ENV MANPATH "${INFER_HOME}/man:${MANPATH}"

# generate a release
RUN cd /infer && \
    make install-with-libs -j \
      BUILD_MODE=opt \
      PATCHELF=patchelf \
      DESTDIR=/infer-release \
      libdir_relative_to_bindir=../lib
RUN tar -cJf /infer-atomer-main.tar.xz -C /infer-release/usr/local . && \
    rm -rf /infer-release

# development setup
RUN cd /infer && eval $(opam env) && make devsetup -j
RUN opam install odoc -y
