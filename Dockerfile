FROM haskell:7.8

WORKDIR /app

RUN echo "deb http://http.debian.net/debian jessie-backports main" > /etc/apt/sources.list.d/jessie-backports.list && \
    apt-get update && \
    apt-get install -y --force-yes docker.io g++

COPY cabal.config /app/cabal.config
COPY wolf.cabal /app/wolf.cabal
RUN cabal update && cabal install --only-dependencies -j4

COPY . /app
RUN cabal install

