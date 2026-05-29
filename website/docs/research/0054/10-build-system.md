---
title: "MEP-54 note 10: Build system"
sidebar_label: "10. Build system"
sidebar_position: 11
description: "go module with a deterministic go.mod/go.sum. SOURCE_DATE_EPOCH=0 plus go build -trimpath for reproducibility. CGo cross-compile via zig cc. GOOS=wasip1 under wasmtime 25+. pkg.go.dev publish dry-run gate."
---

# MEP-54 note 10: Build system

Author: research pass for MEP-54 (Mochi to Go transpiler).
Date: 2026-05-29 (GMT+7).

go module with a deterministic go.mod/go.sum. SOURCE_DATE_EPOCH=0 plus go build -trimpath for reproducibility. CGo cross-compile via zig cc. GOOS=wasip1 under wasmtime 25+. pkg.go.dev publish dry-run gate.

*Full research note content for MEP-54 note 10.*
