.PHONY: clean install

INPUTS := elm.json index.html Makefile src/*.elm

kaleidoscope.js : $(INPUTS)
	elm make src/Main.elm --output=kaleidoscope.js

build/kaleidoscope.js : $(INPUTS)
	elm make src/Main.elm --optimize --output=$@

build/index.html : build/kaleidoscope.js inline.sed
	sed -f inline.sed index.html > $@

install : build/index.html
	curl --netrc-file install-netrc -T build/index.html 'https://myfiles.fastmail.com/nolanw.ca/kaleidoscope/index.html'

clean:
	rm kaleidoscope.js
	rm -r build
