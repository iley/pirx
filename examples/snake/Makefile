PIRXROOT := ../..
PIRXC=$(PIRXROOT)/pirxc
LIBPIRX_PATH=$(PIRXROOT)/stdlib/libpirx.a
RAYLIB_VERSION=5.5

game: game.o libraylib.a
	$(LD) -o $@ $^ $(LIBPIRX_PATH) -lSystem -framework CoreFoundation -framework AppKit -framework IOKit -framework CoreGraphics -framework CoreVideo -syslibroot `xcrun -sdk macosx --show-sdk-path` -arch arm64

game.o: game.s
	$(AS) -arch arm64 -o $@ $<

game.s: raylib.pirx game.pirx
	$(PIRXC) -o $@ $^

clean:
	rm -f game game.s game.o

libraylib.a:
	curl -LO "https://github.com/raysan5/raylib/releases/download/$(RAYLIB_VERSION)/raylib-$(RAYLIB_VERSION)_macos.tar.gz"
	tar -xzvf raylib-$(RAYLIB_VERSION)_macos.tar.gz
	cp raylib-$(RAYLIB_VERSION)_macos/lib/libraylib.a $@
	rm -rf raylib-$(RAYLIB_VERSION)_macos raylib-$(RAYLIB_VERSION)_macos.tar.gz

run: game
	./game

.PHONY: clean run
