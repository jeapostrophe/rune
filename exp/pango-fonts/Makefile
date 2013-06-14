pango-fonts:   main.c
	cc -o $@ -Wall -g $^ \
        `pkg-config --cflags --libs pango` \
        `pkg-config --cflags --libs cairo` \
        `pkg-config --cflags --libs pangocairo` \
        `pkg-config --cflags --libs glib-2.0`
