all: deps
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	runhaskell Setup.hs install

deps:
	cabal update
	cabal install --only-dependencies

clean:
	rm -rf dist
