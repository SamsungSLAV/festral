#!/bin/bash

FESTRAL_BIN_PATH=$1
FARMER_BIN_PATH=$2
MANPAGES_OUT=$3
MAN_INSTALL_PATH="/usr/local/man/man1"
DEB_PATH="deb/festral"
CONTROL="${DEB_PATH}/DEBIAN/control"
POSTINST="${DEB_PATH}/DEBIAN/postinst"

mkdir -p ${DEB_PATH}/usr/bin
mkdir -p ${DEB_PATH}/DEBIAN
mkdir -p ${DEB_PATH}/${MAN_INSTALL_PATH}
cp ${FESTRAL_BIN_PATH}/festral deb/festral/usr/bin/festral
cp ${FARMER_BIN_PATH}/farmer deb/festral/usr/bin/farmer

#install manpages
MANS=$(ls ${MANPAGES_OUT})
for man in ${MANS}
do
    cat ${MANPAGES_OUT}/${man} | gzip > ${DEB_PATH}/${MAN_INSTALL_PATH}/${man}.1
done

echo "Package: festral" > ${CONTROL}
echo "$(grep ^Version Festral.cabal)" >> ${CONTROL}
echo "Maintainer: Uladzislau Harbuz" >> ${CONTROL}
echo "Architecture: all" >> ${CONTROL}
echo "Description: Automated testing system. Client for SLAV stack." >> ${CONTROL}
echo "Depends: curl, git, ssh, libcurl3" >> ${CONTROL}

echo "#!/bin/bash" > ${POSTINST}
echo "festral --bash-completion-script festral >/etc/bash_completion.d/festral" >> ${POSTINST}
echo "farmer --bash-completion-script farmer >/etc/bash_completion.d/farmer" >> ${POSTINST}
chmod +x ${POSTINST}

dpkg-deb --build deb/festral
