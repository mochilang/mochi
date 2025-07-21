#!/bin/bash
set -e
FILES=(group_by_conditional_sum group_by_having group_by_left_join)
OUTDIR=tests/transpiler/x/hs
SRCDIR=tests/vm/valid
for name in "${FILES[@]}"; do
    go run -tags slow scripts/run_transpile.go "$SRCDIR/$name.mochi" "$OUTDIR/$name.hs"
    cp "$SRCDIR/$name.out" "$OUTDIR/$name.out"
    rm -f "$OUTDIR/$name.error"
    echo "processed $name"
done
