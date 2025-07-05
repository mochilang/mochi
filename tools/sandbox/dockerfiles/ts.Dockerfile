FROM golang:1.22 AS builder
WORKDIR /src
COPY . .
RUN go build -o /runner ./tools/sandbox/runner/ts

FROM node:lts
WORKDIR /workspace
COPY --from=builder /runner /usr/local/bin/runner
CMD ["/usr/local/bin/runner"]
