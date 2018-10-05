all: deps
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	runhaskell Setup.hs install

deps:
	cabal update
	cabal install --only-dependencies

CONTROL="deb/festral/DEBIAN/control"
POSTINST="deb/festral/DEBIAN/postinst"
package: all
	mkdir -p deb/festral/usr/bin
	mkdir -p deb/festral/DEBIAN
	cp dist/build/festral/festral deb/festral/usr/bin/festral

	echo "Package: festral" > ${CONTROL}
	echo "$$(grep ^Version Festral.cabal)" >> ${CONTROL}
	echo "Maintainer: Uladzislau Harbuz" >> ${CONTROL}
	echo "Architecture: all" >> ${CONTROL}
	echo "Description: Automated testing system. Client for SLAV stack." >> ${CONTROL}
	echo "Depends: curl, git, ssh" >> ${CONTROL}

	echo "#!/bin/bash" > ${POSTINST}
	echo "festral --bash-completion-script festral >/etc/bash_completion.d/festral" >> ${POSTINST}
	chmod +x ${POSTINST}

	dpkg-deb --build deb/festral

clean:
	rm -rf dist
	rm -rf deb
