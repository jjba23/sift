# sift --- a container image for the formatter, the linter and the server
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# sift is a shell launcher, a directory of Scheme, and Guile.  That is all this
# image is: Debian's Guile 3.0, the library, and a symlink in PATH.  There is no
# build step, because there is nothing to build -- the program is source, and
# the first run compiles it into a cache the image already holds.
#
#   docker build --tag sift .
#   docker run --rm --volume "$PWD:/src" sift check .
#
# (Any engine that reads a Dockerfile will do: `podman build --tag sift .` is
# the same image, and `maak package-docker` is this command with the tags
# already on.)
#
# The image is Debian and not a Scheme distribution because the point of it is
# the *other* end: on macOS there is no Guile in `brew install guile` that a
# script can rely on, and no `guix install` without Guix, but there is Docker
# Desktop.  One `docker run` is a sift that works, and the volume is the only
# thing that has to be right.

FROM debian:stable-slim

LABEL org.opencontainers.image.title="sift" \
      org.opencontainers.image.description="A dialect-aware formatter, linter and language server for Scheme, Emacs Lisp and Common Lisp" \
      org.opencontainers.image.source="https://github.com/jjba23/sift" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.documentation="https://github.com/jjba23/sift/blob/master/README.org"

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        ca-certificates \
        guile-3.0 \
    && rm -rf /var/lib/apt/lists/*

# The launcher finds the library by looking beside itself, so the two keep the
# layout the checkout has: /opt/sift/bin/sift and /opt/sift/src/sift/*.scm.
COPY bin/ /opt/sift/bin/
COPY src/ /opt/sift/src/

# `sift version` here is not a formality: it is what compiles the library into
# /root/.cache/guile at build time, so that the first `docker run` answers as
# quickly as the second.
RUN ln --symbolic /opt/sift/bin/sift /usr/local/bin/sift \
    && sift version

WORKDIR /src

ENTRYPOINT ["sift"]
CMD ["--help"]
