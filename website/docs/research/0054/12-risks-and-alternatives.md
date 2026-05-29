---
title: "MEP-54 note 12: Risks and alternatives"
sidebar_label: "12. Risks and alternatives"
sidebar_position: 13
description: "Risk register: CGo cross-compile detection fragility, goroutine leak on agent panic, net/http stub under GOOS=wasip1, cassette drift, tagged-union verbosity, go.sum churn. Rejected alternatives: go/ast (too coupled to Go internals), generics for sum types (too verbose for simple cases), no-CGo FFI (breaks extern C), tokio-style async (no equivalent in Go)."
---

# MEP-54 note 12: Risks and alternatives

Author: research pass for MEP-54 (Mochi to Go transpiler).
Date: 2026-05-29 (GMT+7).

Risk register: CGo cross-compile detection fragility, goroutine leak on agent panic, net/http stub under GOOS=wasip1, cassette drift, tagged-union verbosity, go.sum churn. Rejected alternatives: go/ast (too coupled to Go internals), generics for sum types (too verbose for simple cases), no-CGo FFI (breaks extern C), tokio-style async (no equivalent in Go).

*Full research note content for MEP-54 note 12.*
