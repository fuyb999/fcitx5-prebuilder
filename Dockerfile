FROM fcitx5-prebuilder-base:latest

RUN mkdir -p /tmp/sdk/cmake/3.31.6/bin && \
    ln -sf /usr/bin/cmake /tmp/sdk/cmake/3.31.6/bin/cmake

COPY . .

RUN ./build-cabal && \
    cabal run prebuilder -- --verbose everything
