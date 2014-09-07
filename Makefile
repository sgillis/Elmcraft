all: clean build

clean:
	rm -rf build

build:
	cabal exec elm -- --make --only-js --src-dir=src Main.elm
	cabal exec elm-server
