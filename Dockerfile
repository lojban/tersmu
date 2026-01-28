## Build and runtime image for the Haskell tersmu parser
##
## The original single-stage image was huge because it shipped:
##  - the full GHC+cabal toolchain (from haskell:9.8)
##  - cabal store/build artifacts
##  - the whole source tree
##
## This multi-stage Dockerfile keeps all build tooling in the builder stage,
## then copies only the installed binaries + data-files into a slim runtime.

FROM haskell:9.8 AS builder

## Additional tools needed at build time:
##  - python3: to generate .pappy from canonical .pest + .pappy.rhs
RUN apt-get update && apt-get install -y --no-install-recommends \
    darcs \
    gcc \
    make \
    python3 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

## Build from the current checkout (not from a remote clone)
COPY . .

## Generate .pappy from canonical Pest grammar (.pest + .pappy.rhs) before Pappy/Haskell build
RUN python3 scripts/gen_pappy.py Lojban.pest Lojban.pappy.rhs -o Lojban.pappy \
 && python3 scripts/gen_pappy.py Morphology.pest Morphology.pappy.rhs -o Morphology.pappy

## Fetch/build the patched Pappy tool, then generate the Haskell modules
## required by cabal (Lojban.hs, Morphology.hs, Pappy/Parse.hs).
RUN make pappy/pappy/pappy \
 && make Pappy/Parse.hs Lojban.hs Morphology.hs

## Install just the executables into a clean directory so we can copy them
## into a tiny runtime image. This avoids shipping /root/.cabal (huge store).
RUN cabal update
RUN mkdir -p /opt/tersmu/bin \
 && cabal install \
      exe:tersmu \
      exe:tersmu-server \
      --install-method=copy \
      --installdir=/opt/tersmu/bin \
      --overwrite-policy=always

## Optional: strip symbols to reduce binary size (safe to ignore if strip missing)
RUN strip /opt/tersmu/bin/tersmu /opt/tersmu/bin/tersmu-server || true

FROM debian:bookworm-slim AS runtime

## Runtime shared libraries commonly needed by GHC-produced binaries.
## (Exact set depends on linking; these are small compared to GHC itself.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/tersmu/bin/tersmu /usr/local/bin/tersmu
COPY --from=builder /opt/tersmu/bin/tersmu-server /usr/local/bin/tersmu-server

EXPOSE 8080

## By default, run the HTTP REST API. To run the CLI instead:
##   docker run --rm -it --entrypoint tersmu tersmu examples/1.jbo
CMD ["tersmu-server"]

