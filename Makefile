.PHONY: clean

kaleidoscope.js : elm.json index.html Makefile src/*.elm
	elm make src/Main.elm --output=kaleidoscope.js

clean:
	rm kaleidoscope.js
