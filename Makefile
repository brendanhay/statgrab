all: build

build: .conf
	cabal build

install:
	cabal install -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf dist

lint:
	hlint src

.conf:
	cabal configure && touch .conf
