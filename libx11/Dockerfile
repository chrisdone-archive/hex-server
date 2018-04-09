FROM debian:9-slim
MAINTAINER Chris Done

COPY sources.list /etc/apt/sources.list
RUN apt-get update

RUN apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y \
    build-essential dpkg-dev
RUN apt-get build-dep libx11 -y

RUN mkdir /x11;
RUN cd /x11 && apt-get source libx11
RUN cd /x11/libx11-1.6.4 && ./configure
RUN cd /x11/libx11-1.6.4 && make -j4
