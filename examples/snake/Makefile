PIRXROOT := ../..
PIRXC=$(PIRXROOT)/pirxc
LIBPIRX_PATH=$(PIRXROOT)/stdlib/libpirx.a

game: game.o libraylib.a
	$(LD) -o $@ $^ $(LIBPIRX_PATH) -lSystem -framework CoreFoundation -framework AppKit -framework IOKit -framework CoreGraphics -framework CoreVideo -syslibroot `xcrun -sdk macosx --show-sdk-path` -arch arm64

game.o: game.s
	$(AS) -arch arm64 -o $@ $<

game.s: raylib.pirx game.pirx
	$(PIRXC) -o $@ $^

clean:
	rm -f game game.s game.o

libraylib.a:
	curl -LO "https://github.com/raysan5/raylib/releases/download/5.5/raylib-5.5_macos.tar.gz"
	tar -xzvf raylib-5.5_macos.tar.gz
	cp raylib-5.5_macos/lib/libraylib.a $@
	rm -rf raylib-5.5_macos raylib-5.5_macos.tar.gz

run: game
	./game

.PHONY: clean run
