DEST?=./bin/

build:
	cabal build
	@mkdir -p ./bin
	cp ./dist/build/erd/erd $(DEST)

clean:
	rm -rf {dist,bin}
	cabal configure

docs:
	cabal haddock --hyperlink-source --executables
	rscp ./dist/doc/html/erd/erd Geils:~/www/burntsushi.net/public_html/docs/haddock/

push:
	git push origin master
	git push github master
