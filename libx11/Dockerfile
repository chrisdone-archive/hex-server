FROM debian:9-slim
MAINTAINER Chris Done

COPY sources.list /etc/apt/sources.list
RUN apt-get update

RUN apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y \
    build-essential dpkg-dev
RUN apt-get build-dep libxcb1 -y

RUN mkdir /x11;
RUN cd /x11 && apt-get source libxcb1
RUN cd /x11 && ls
RUN cd /x11/libxcb-1.12 && ./configure
RUN cd /x11/libxcb-1.12 && make -j4

COPY xcb_conn.c /x11/libxcb-1.12/src

RUN cd /x11/libxcb-1.12 && make -j4

RUN mkdir /xcb-out
RUN cd /x11/libxcb-1.12 && make install DESTDIR=/xcb-out

RUN apt-get purge libxcb1 -y && apt-get autoremove -y

COPY guest-run.sh /
COPY xcb.c  /
