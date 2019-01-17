FROM haskell:8
LABEL maintainer="Uladzislau Harbuz <u.harbuz@samsung.com>"

RUN apt update
RUN apt install -y libghc-curl-dev

COPY . festral/

WORKDIR  festral

RUN make package DEPS_OPTS=--force-reinstalls
