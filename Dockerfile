FROM haskell:8.8.4-buster

COPY stack.yaml .
COPY stack.yaml.lock .
COPY Setup.hs .
COPY yinlang.cabal .
COPY LICENSE .
COPY README.md .
COPY ChangeLog.md .

RUN stack build --dependencies-only

COPY src/ src/
COPY app/ app/
COPY test/ test/
COPY std.yin std.yin

RUN stack install

FROM debian:buster

COPY --from=0 /root/.local/bin/yin /usr/local/bin

COPY std.yin .

CMD yin
