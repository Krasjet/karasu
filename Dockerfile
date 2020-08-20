# Waiting for texlive 2020
# # --- build dvisvgm, probably no longer needed ---
# FROM ubuntu:19.10 AS build-dvisvgm

# ENV DEBIAN_FRONTEND=noninteractive

# RUN apt-get update && apt-get install --no-install-recommends -yq \
#     autotools-dev \
#     libfreetype6-dev \
#     libgs-dev \
#     libkpathsea-dev \
#     libz-dev \
#     python-lxml \
#     asciidoc \
#     xmlto \
#     xsltproc \
#     g++ \
#     ca-certificates \
#     git \
#     autoconf \
#     automake \
#     libtool && \
#     apt-get autoclean autoremove && \
#     rm -rf /var/lib/apt/lists/* \
#            /tmp/* \
#            /var/tmp/*

# WORKDIR /build

# # we need a newer version of dvisvgm for ghostscript compatibility and the
# # exact-bbox option. This might not be needed when texlive-2020 is released
# RUN git clone -b 2.9.1 https://github.com/mgieseki/dvisvgm.git
# RUN cd dvisvgm/ && \
#     ./autogen.sh && \
#     ./configure --prefix="/build/" --enable-bundled-libs && \
#     make install

# --- build karasu ---
FROM fpco/stack-build-small:lts-15.13 AS build-karasu

WORKDIR /build

# we only need these two to build the dependencies
COPY stack.yaml package.yaml /build/
COPY fl.tr/ /build/fl.tr/
COPY libkst/ /build/libkst/
COPY pandoc-utils/ /build/pandoc-utils/
# because of pandoc, this will take a long time
RUN stack build --dependencies-only

# start building karasu
COPY . /build/

RUN stack build
RUN mv "$(stack path --local-install-root)/bin" /build/bin

# --- the actual app ---
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yq && \
    apt-get install --no-install-recommends -y texlive-full dvisvgm locales && \
    apt-get --purge remove -y .\*-doc$ && \
    apt-get autoclean autoremove && \
    rm -rf /var/lib/apt/lists/* \
           /tmp/* \
           /var/tmp/*

# If locale is not set, haskell will have some problem handling unicode
# characters
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

WORKDIR /app
# use pre-compiled karasu
COPY --from=build-karasu /build/bin/karasu /app/
COPY --from=build-karasu /build/templates/ /app/templates/
# use pre-compiled dvisvgm
# COPY --from=build-dvisvgm /build/bin/dvisvgm /usr/bin/dvisvgm

ENTRYPOINT /app/karasu
