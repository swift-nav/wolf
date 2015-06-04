FROM mfine/haskell:7.8

WORKDIR /app

RUN apt-get update && apt-get -y --force-yes install curl && curl -sSL https://get.docker.com/ | sh

COPY flow.cabal /app/flow.cabal
RUN cabal update && cabal install --only-dependencies -j4

COPY . /app
RUN cabal configure && cabal install
