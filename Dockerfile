FROM fcitx5-prebuilder-base:latest

COPY . .

RUN ./build-cabal && \
    cabal run prebuilder -- --verbose everything
