# --- build dvisvgm ---
FROM ubuntu:19.10 AS build-dvisvgm

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install --no-install-recommends -yq \
    autotools-dev \
    libfreetype6-dev \
    libgs-dev \
    libkpathsea-dev \
    libz-dev \
    python-lxml \
    asciidoc \
    xmlto \
    xsltproc \
    g++ \
    ca-certificates \
    git \
    autoconf \
    automake \
    libtool

WORKDIR /build

# we need a newer version to use the exact-bbox option
# this might not be needed when texlive-2020 is released
RUN git clone -b 2.9.1 https://github.com/mgieseki/dvisvgm.git
RUN cd dvisvgm/ && \
    ./autogen.sh && \
    ./configure --prefix="/build/" --enable-bundled-libs && \
    make install

# --- build dependencies ---
FROM fpco/stack-build-small:lts-14.27 as dependencies

WORKDIR /build

# we only need these two to build the dependencies
COPY stack.yaml package.yaml /build/

# because of pandoc, this will take a long time
RUN stack build --dependencies-only

# --- build karasu ---
FROM fpco/stack-build-small:lts-14.27 as build-karasu

# the cache from dependencies
COPY --from=dependencies /root/.stack /root/.stack
COPY . /build/

WORKDIR /build

RUN stack build
RUN mv "$(stack path --local-install-root)/bin" /build/bin

# --- the actual app ---
FROM ubuntu:19.10

RUN DEBIAN_FRONTEND=noninteractive apt-get update -yq && \
    apt-get install --no-install-recommends -y texlive-full && \
    apt-get --purge remove -y .\*-doc$ && \
    apt-get autoclean autoremove && \
    rm -rf /var/lib/apt/lists/* \
           /tmp/* \
           /var/tmp/*

WORKDIR /app
# use pre-compiled karasu
COPY --from=build-karasu /build/bin/karasu /app/
# use pre-compiled dvisvgm
COPY --from=build-dvisvgm /build/bin/dvisvgm /usr/bin/dvisvgm

ENTRYPOINT /app/karasu
