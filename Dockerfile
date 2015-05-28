FROM mfine/haskell:7.8

WORKDIR /app

COPY flow.cabal /app/flow.cabal
RUN cabal update && cabal install --only-dependencies -j4

COPY . /app
RUN cabal configure && cabal install

