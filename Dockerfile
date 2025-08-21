FROM fcitx5-prebuilder-base:latest

RUN mkdir -p /tmp/sdk/cmake/3.31.6/bin && \
    ln -sf /usr/bin/cmake /tmp/sdk/cmake/3.31.6/bin/cmake


RUN apt-get install -y locales language-pack-zh-hans fonts-wqy-zenhei && \
    sed -i '/zh_CN.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen && \
    update-locale LANG=zh_CN.UTF-8

ENV LANG=zh_CN.UTF-8
ENV LC_ALL=zh_CN.UTF-8

COPY . .

RUN ./build-cabal && \
    cabal run prebuilder -- --verbose libime
