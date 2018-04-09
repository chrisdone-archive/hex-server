# echo compiling with /xcb-out/usr/local/include -L/xcb-out/usr/local/lib
# gcc xcb.c -lxcb -static -lpthread -lXau -lXdmcp -o xcb -I/xcb-out/usr/local/include -L/xcb-out/usr/local/lib
# echo running xcb
# ./xcb


echo compiling with /xcb-out/usr/local/include -L/xcb-out/usr/local/lib
gcc x11.c -lX11 -lm -lxcb -static -lpthread -lXau -lXdmcp -o x11-bin -I/xcb-out/usr/local/include -L/xcb-out/usr/local/lib
echo running x11
./x11-bin
