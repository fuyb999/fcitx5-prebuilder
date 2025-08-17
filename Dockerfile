FROM fcitx5-prebuilder-base:latest

COPY . .

RUN nix develop
