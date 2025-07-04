include config.mk

all: install

clean:
	[ -f "build/*" ] && rm -r build/*

install:
	cd build && cmake .. && make
	cp build/xkb-switch ${PREFIX}/bin
