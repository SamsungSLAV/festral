#
# Copyright (c) 2018-2019 Samsung Electronics Co., Ltd All Rights Reserved
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

BUILD_IMAGE_NAME =festral-build-img
CONTAINER_NAME =festral-container
BINARIES =farmer festral
BUILD_OUT =bin
DEB_NAME =festral.deb
DISTRIBUTION_PATH =Distribution
MANPAGES_PATH=${DISTRIBUTION_PATH}/ManPages
MAN_PAGES_OUT =${MANPAGES_PATH}/manpages
BUILD_PREFIX =dist/build
FESTRAL_BIN_PATH =${BUILD_PREFIX}/festral
FARMER_BIN_PATH =${BUILD_PREFIX}/farmer

all: build package

build: deps configure
	cabal ${CABAL_PREFIX}build
	mkdir -p ${BUILD_OUT}
	cp ${FESTRAL_BIN_PATH}/festral ${BUILD_OUT}/
	cp ${FARMER_BIN_PATH}/farmer ${BUILD_OUT}/

deps:
	cabal ${CABAL_PREFIX}update
	cabal ${CABAL_PREFIX}install --only-dependencies ${DEPS_OPTS}

manpages: build
	${MANPAGES_PATH}/manpage-generator.sh ${BUILD_OUT} ${MAN_PAGES_OUT} ${MANPAGES_PATH}

docs: configure
	cabal haddock

configure:
	cabal ${CABAL_PREFIX}configure

docker: ${BINARIES} docker-deb
	docker rm ${CONTAINER_NAME}

docker-build:
	docker build --tag ${BUILD_IMAGE_NAME} .

docker-container: docker-build
	docker create --name ${CONTAINER_NAME} ${BUILD_IMAGE_NAME}

docker-clean:
	docker rm ${CONTAINER_NAME}
	docker rmi ${BUILD_IMAGE_NAME}
	rm -rf ${BUILD_OUT}

${BINARIES}: docker-container | build-outdir
	docker cp "${CONTAINER_NAME}:/festral/${BUILD_PREFIX}/$@/$@" "${BUILD_OUT}/$@"

docker-deb: docker-container
	docker cp "${CONTAINER_NAME}:/festral/deb/${DEB_NAME}" "${BUILD_OUT}/${DEB_NAME}"

build-outdir:
	mkdir -p ${BUILD_OUT}

package: build manpages
	./Distribution/make_debian.sh ${FESTRAL_BIN_PATH} ${FARMER_BIN_PATH} ${MAN_PAGES_OUT}
	cp deb/${DEB_NAME} ${BUILD_OUT}/

clean:
	rm -rf dist
	rm -rf deb
	rm -rf ${BUILD_OUT}
	rm -rf ${MAN_PAGES_OUT}
