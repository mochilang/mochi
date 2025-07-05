FROM golang:1.22 AS builder
WORKDIR /src
COPY . .
RUN go build -o /runner ./tools/sandbox/runner/pas

FROM debian:bookworm
RUN apt-get update \
    && apt-get install -y fpc \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /workspace
COPY --from=builder /runner /usr/local/bin/runner
CMD ["/usr/local/bin/runner"]
