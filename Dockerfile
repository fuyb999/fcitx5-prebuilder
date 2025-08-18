FROM fcitx5-prebuilder-base:latest

RUN mkdir -p /tmp/sdk/cmake/3.31.6/bin && \
    ln -sf /usr/bin/cmake /tmp/sdk/cmake/3.31.6/bin/cmake

RUN apt-get install -y gperf binutils-x86-64-linux-gnu binutils-aarch64-linux-gnu binutils-arm-linux-gnueabihf binutils-i686-linux-gnu

COPY . .

RUN cabal run prebuilder -- --verbose everything
