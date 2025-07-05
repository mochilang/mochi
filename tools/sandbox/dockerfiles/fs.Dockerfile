FROM golang:1.22 AS builder
WORKDIR /src
COPY . .
RUN go build -o /runner ./tools/sandbox/runner/fs

FROM mcr.microsoft.com/dotnet/sdk:8.0
WORKDIR /workspace
COPY --from=builder /runner /usr/local/bin/runner
CMD ["/usr/local/bin/runner"]
