FROM golang AS build
ADD . /go/src/github.com/koki/ci-lang/scheduler
RUN go install github.com/koki/ci-lang/scheduler

FROM ubuntu:16.04
RUN mkdir -p /opt/koki
ARG BINARY_PATH
WORKDIR /opt/koki
COPY --from=build /go/bin/scheduler /opt/koki/scheduler
CMD ["/opt/koki/scheduler"]
