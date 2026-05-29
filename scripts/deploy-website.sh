#!/usr/bin/env bash
# Syncs website/ from mochilang/mochi to mochilang/docs and triggers deploy.
#
# Usage:
#   ./scripts/deploy-website.sh                  # local run (uses gh auth)
#   GH_TOKEN=<pat> ./scripts/deploy-website.sh   # CI run
#
# Requires: git, rsync
set -euo pipefail

MOCHI_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
WEBSITE_SRC="$MOCHI_ROOT/website"
DOCS_REPO="mochilang/docs"

# Build authenticated URL when token is provided
if [ -n "${GH_TOKEN:-}" ]; then
  REPO_URL="https://x-access-token:${GH_TOKEN}@github.com/${DOCS_REPO}.git"
else
  REPO_URL="https://github.com/${DOCS_REPO}.git"
fi

TMPDIR_WORK="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_WORK"' EXIT

echo "==> Cloning mochilang/docs..."
git clone --depth=1 "$REPO_URL" "$TMPDIR_WORK/docs"

echo "==> Syncing website/ content..."
rsync -a --delete \
  --exclude='.git' \
  --exclude='.DS_Store' \
  --exclude='node_modules' \
  --exclude='.docusaurus' \
  --exclude='build' \
  --exclude='LICENSE' \
  --exclude='README.md' \
  "$WEBSITE_SRC/" \
  "$TMPDIR_WORK/docs/"

echo "==> Syncing releases/ content..."
rsync -a --delete \
  --exclude='.git' \
  "$MOCHI_ROOT/releases/" \
  "$TMPDIR_WORK/docs/releases/"

cd "$TMPDIR_WORK/docs"
git config user.name "mochi-bot"
git config user.email "bot@mochi-lang.dev"
git add -A

if git diff --staged --quiet; then
  echo "==> No changes to sync, docs repo is up to date."
  exit 0
fi

MOCHI_SHA="$(git -C "$MOCHI_ROOT" rev-parse --short HEAD)"
git commit -m "sync: website from mochi@${MOCHI_SHA}"
git push
echo "==> Pushed. Deploy triggered on mochilang/docs."
