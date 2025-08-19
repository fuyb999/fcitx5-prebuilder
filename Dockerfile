FROM fcitx5-prebuilder-base:latest

RUN mkdir -p /tmp/sdk/cmake/3.31.6/bin && \
    ln -sf /usr/bin/cmake /tmp/sdk/cmake/3.31.6/bin/cmake


ENV LANG=zh_CN.UTF-8
ENV LANGUAGE=zh_CN:zh
ENV LC_ALL=zh_CN.UTF-8
RUN apt install -y zstd language-pack-zh-hans fonts-wqy-zenhei

COPY . .

RUN ./build-cabal && \
    cabal run prebuilder -- --verbose everything
