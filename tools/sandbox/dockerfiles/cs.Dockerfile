FROM golang:1.22 AS builder
WORKDIR /src
COPY . .
RUN go build -o /runner ./tools/sandbox/runner/cs

FROM mcr.microsoft.com/dotnet/sdk:8.0
RUN apt-get update \
    && apt-get install -y mono-complete \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /workspace
COPY --from=builder /runner /usr/local/bin/runner
CMD ["/usr/local/bin/runner"]
