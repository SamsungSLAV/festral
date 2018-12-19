#
# Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
#
# Author: Uladzislau Harbuz <u.harbuz@samsung.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License
#

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
	cp dist/build/farmer/farmer deb/festral/usr/bin/farmer

	echo "Package: festral" > ${CONTROL}
	echo "$$(grep ^Version Festral.cabal)" >> ${CONTROL}
	echo "Maintainer: Uladzislau Harbuz" >> ${CONTROL}
	echo "Architecture: all" >> ${CONTROL}
	echo "Description: Automated testing system. Client for SLAV stack." >> ${CONTROL}
	echo "Depends: curl, git, ssh" >> ${CONTROL}

	echo "#!/bin/bash" > ${POSTINST}
	echo "festral --bash-completion-script festral >/etc/bash_completion.d/festral" >> ${POSTINST}
	echo "farmer --bash-completion-script farmer >/etc/bash_completion.d/farmer" >> ${POSTINST}
	chmod +x ${POSTINST}

	dpkg-deb --build deb/festral

clean:
	rm -rf dist
	rm -rf deb
