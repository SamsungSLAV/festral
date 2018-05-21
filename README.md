#Festral 
It is a simple client for tests management using Weles as test server.

----------
###How to build
Installation scripts presented below are made on Ubuntu 16.04.

`Festral` is written in Haskell and for building it from sources your need to have installed `haskell-platform` package for your distribution:

> $ sudo apt install haskell-platform

The next step is to install `cabal` package manager which will configure `Festral's` sources, resolve its dependencies, build and install package for you:

> \$ sudo apt install cabal-install 
> \$ cabal update

Now you can build the package. Go to the root directory of the `Festral` sources (it contains Setup.hs file) and run:

> \$ runhaskell Setup.hs configure --user
> \$ runhaskell Setup.hs build
> \$ runhaskell Setup.hs install

If build finished with success, you will find festral executable in the directory `dist/build/festral/festral`

