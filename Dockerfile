FROM fpco/stack-build:lts-10.2 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/koki
ARG BINARY_PATH
WORKDIR /opt/koki
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-10.2/8.2.2/bin .
CMD ["/opt/koki/ci-lang-exe"]