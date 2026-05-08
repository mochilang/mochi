#!/usr/bin/env sh
# Mochi installer.
#
# Usage:
#   curl -fsSL https://get.mochi-lang.dev | sh
#
# Environment overrides:
#   MOCHI_VERSION   Pin a specific tag (e.g. v0.10.82). Default: latest release.
#   MOCHI_INSTALL   Install prefix. Default: $HOME/.mochi
#
# Installs the mochi binary to $MOCHI_INSTALL/bin and prints the line to add
# to your shell profile. Existing binaries are overwritten.

set -eu

REPO="mochilang/mochi"
INSTALL_DIR="${MOCHI_INSTALL:-$HOME/.mochi}"
BIN_DIR="$INSTALL_DIR/bin"
TARGET="$BIN_DIR/mochi"

red()    { printf "\033[31m%s\033[0m" "$1"; }
green()  { printf "\033[32m%s\033[0m" "$1"; }
yellow() { printf "\033[33m%s\033[0m" "$1"; }
bold()   { printf "\033[1m%s\033[0m" "$1"; }

die() {
  printf "%s %s\n" "$(red error:)" "$1" >&2
  exit 1
}

need() {
  command -v "$1" >/dev/null 2>&1 || die "'$1' is required but not installed"
}

need uname
need tar
if command -v curl >/dev/null 2>&1; then
  fetch() { curl -fsSL "$1" -o "$2"; }
  fetch_stdout() { curl -fsSL "$1"; }
elif command -v wget >/dev/null 2>&1; then
  fetch() { wget -qO "$2" "$1"; }
  fetch_stdout() { wget -qO- "$1"; }
else
  die "neither curl nor wget is available"
fi

raw_os=$(uname -s)
case "$raw_os" in
  Linux)  OS="Linux" ;;
  Darwin) OS="Darwin" ;;
  *) die "unsupported OS: $raw_os (Linux and Darwin only). On Windows, download the binary from https://github.com/$REPO/releases" ;;
esac

raw_arch=$(uname -m)
case "$raw_arch" in
  x86_64|amd64)   ARCH="x86_64" ;;
  arm64|aarch64)  ARCH="arm64" ;;
  *) die "unsupported architecture: $raw_arch (need x86_64 or arm64)" ;;
esac

VERSION="${MOCHI_VERSION:-}"
if [ -z "$VERSION" ]; then
  printf "Looking up latest release... "
  VERSION=$(fetch_stdout "https://api.github.com/repos/$REPO/releases/latest" \
    | awk -F'"' '/"tag_name":/ { print $4; exit }')
  [ -n "$VERSION" ] || die "could not detect latest version (set MOCHI_VERSION to override)"
  printf "%s\n" "$(green "$VERSION")"
fi

ASSET="mochi_${OS}_${ARCH}.tar.gz"
URL="https://github.com/$REPO/releases/download/$VERSION/$ASSET"

TMP=$(mktemp -d 2>/dev/null || mktemp -d -t mochi)
trap 'rm -rf "$TMP"' EXIT INT TERM

printf "Downloading %s\n" "$URL"
fetch "$URL" "$TMP/$ASSET" || die "download failed (asset may not exist for this platform)"

printf "Extracting...\n"
tar -xzf "$TMP/$ASSET" -C "$TMP" || die "extraction failed"

BIN_SRC=""
for candidate in "$TMP/mochi" "$TMP/bin/mochi"; do
  if [ -f "$candidate" ]; then
    BIN_SRC="$candidate"
    break
  fi
done
if [ -z "$BIN_SRC" ]; then
  BIN_SRC=$(find "$TMP" -maxdepth 4 -type f -name mochi 2>/dev/null | head -n 1)
fi
[ -n "$BIN_SRC" ] || die "could not locate mochi binary inside $ASSET"

mkdir -p "$BIN_DIR"
install -m 0755 "$BIN_SRC" "$TARGET" 2>/dev/null || {
  cp "$BIN_SRC" "$TARGET"
  chmod 0755 "$TARGET"
}

printf "\n%s mochi %s -> %s\n" "$(green "Installed")" "$VERSION" "$(bold "$TARGET")"

case ":${PATH:-}:" in
  *":$BIN_DIR:"*)
    printf "%s is already on your PATH.\n" "$BIN_DIR"
    ;;
  *)
    printf "\n%s\n" "$(yellow "Add this line to your shell profile:")"
    printf "  export PATH=\"%s:\$PATH\"\n" "$BIN_DIR"
    ;;
esac

printf "\nVerify with:\n"
printf "  mochi --version\n"
printf "  mochi run -e 'print(\"ready\")'\n"
