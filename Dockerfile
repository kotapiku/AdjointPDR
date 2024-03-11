FROM haskell:9.4.8

RUN apt-get update \
  && apt-get install -y python3-pip gawk \
  && pip3 install z3-solver==4.11.2 \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /root/artifact
COPY . .

RUN cabal update && cabal install --lib

CMD ["cabal", "bench"]
