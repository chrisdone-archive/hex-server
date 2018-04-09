all: Dockerfile
	docker-machine scp Dockerfile custom-linux: > /dev/null \
	&& docker-machine scp sources.list custom-linux: > /dev/null \
	&& docker-machine scp xcb.c custom-linux: > /dev/null \
	&& docker-machine scp guest-run.sh custom-linux: > /dev/null \
	&& docker-machine scp ~/libxcb/src/xcb_conn.c custom-linux: > /dev/null \
	&& docker-machine ssh custom-linux docker image build . -t x11
