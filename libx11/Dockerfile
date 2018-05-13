FROM debian:9-slim
MAINTAINER Chris Done

COPY sources.list /etc/apt/sources.list
RUN apt-get update

RUN apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y \
    build-essential dpkg-dev
RUN apt-get build-dep libxcb1 libx11 -y

RUN mkdir /x11;
RUN cd /x11 && apt-get source libxcb1
RUN cd /x11 && apt-get source libx11
RUN ls /x11

RUN mkdir /xcb-out

RUN cd /x11/libxcb-1.12 && ./configure
COPY xcb_conn.c /x11/libxcb-1.12/src
COPY xcb_util.c /x11/libxcb-1.12/src
RUN cd /x11/libxcb-1.12 && make install -j4 DESTDIR=/xcb-out

RUN cd /x11/libx11-1.6.4 && C_INCLUDE_PATH=/xcb-out/usr/local/include LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/xcb-out/usr/local/lib ./configure

RUN cd /x11/libx11-1.6.4 && C_INCLUDE_PATH=/xcb-out/usr/local/include LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/xcb-out/usr/local/lib make install -j4  DESTDIR=/xcb-out

RUN apt-get install emacs chromium -y

COPY OpenDis.c /x11/libx11-1.6.4/src
COPY QuColors.c /x11/libx11-1.6.4/src
COPY xcb_io.c /x11/libx11-1.6.4/src
COPY xcb_disp.c /x11/libx11-1.6.4/src

RUN cd /x11/libx11-1.6.4 && C_INCLUDE_PATH=/xcb-out/usr/local/include LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/xcb-out/usr/local/lib make install -j4  DESTDIR=/xcb-out

COPY xcb.c /
COPY x11.c /
COPY guest-run.sh /
