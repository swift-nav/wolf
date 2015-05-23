FROM haskell:7.8

WORKDIR /app

COPY flow.cabal /app/flow.cabal
RUN cabal update
RUN cabal install --only-dependencies -j4

COPY . /app
RUN cabal configure
RUN cabal install

