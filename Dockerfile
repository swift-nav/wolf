FROM haskell:7.10

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --force-yes wget && \
    wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/debian/fpco.key | apt-key add - && \
    echo 'deb http://download.fpcomplete.com/debian/jessie stable main'| tee /etc/apt/sources.list.d/fpco.list && \
    echo "deb http://http.debian.net/debian jessie-backports main" | tee /etc/apt/sources.list.d/jessie-backports.list && \
    apt-get update && \
    apt-get install -y --force-yes stack docker.io g++

ENV PATH /root/.local/bin:$PATH

COPY LICENSE Setup.hs wolf.cabal stack.yaml /app/
RUN stack build wolf --only-dependencies

COPY src /app/src
RUN stack build wolf --copy-bins

