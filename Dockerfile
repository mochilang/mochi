FROM golang:1.24 AS build

WORKDIR /src
COPY . .

# Build mochi binary with trimpath and stripped symbols
RUN --mount=type=cache,target=/root/.cache/go-build \
    --mount=type=cache,target=/go/pkg/mod \
    go build -trimpath -ldflags="-s -w" -o /mochi ./cmd/mochi

# Final minimal image
FROM debian:bookworm-slim

# Install CA certificates and ensure update
RUN apt-get update \
 && apt-get install -y ca-certificates \
 && update-ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Add mochi binary
COPY --from=build /mochi /usr/bin/mochi

# Set default entrypoint so all subcommands are supported
ENTRYPOINT ["/usr/bin/mochi"]
