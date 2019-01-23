FROM haskell:8
LABEL maintainer="Uladzislau Harbuz <u.harbuz@samsung.com>"

RUN apt update
RUN apt install -y libghc-curl-dev help2man

COPY . festral/

WORKDIR  festral

RUN make package docs DEPS_OPTS=--force-reinstalls
