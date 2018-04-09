echo compiling with /xcb-out/usr/local/include -L/xcb-out/usr/local/lib
gcc xcb.c -lxcb -static -lpthread -lXau -lXdmcp -o xcb -I/xcb-out/usr/local/include -L/xcb-out/usr/local/lib
echo running
./xcb
