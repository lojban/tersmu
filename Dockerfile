## Build and runtime image for the Haskell tersmu parser
##
## We use the official haskell image so that GHC and cabal-install are
## available and up-to-date, and then build the parser from the *local*
## source tree rather than re-cloning from Git.

FROM haskell:9.8

## Additional tools needed at build time:
##  - darcs: to fetch the patched Pappy parser used by the Makefile
##  - make: already present in the base image, but kept here for clarity
RUN apt-get update && apt-get install -y \
    darcs \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

## Build from the current checkout (not from a remote clone)
COPY . .

## Ensure the cabal bin directory is on PATH for both build and runtime.
ENV PATH=/root/.cabal/bin:$PATH

## Build and install tersmu and tersmu-server. Tests can be run separately
## (see test_all_examples.sh and test_api_examples.sh).
RUN cabal update \
 && make install

EXPOSE 8080

## By default, run the HTTP REST API. To run the CLI instead:
##   docker run --rm -it --entrypoint tersmu tersmu examples/1.jbo
## To run the API and validate examples:
##   docker run -d -p 8080:8080 --name tersmu-api tersmu && ./test_api_examples.sh
CMD ["tersmu-server"]

