---
title: "MEP-54 note 02: Design philosophy"
sidebar_label: "02. Design philosophy"
sidebar_position: 3
description: "Why Go 1.22 as the floor, why goroutine + buffered-channel agents instead of a custom actor runtime, why tagged-struct union instead of Go generics for sum types, why CGo for FFI, and why the runtime package is a plain go module rather than a framework."
---

# MEP-54 note 02: Design philosophy

Author: research pass for MEP-54 (Mochi to Go transpiler).
Date: 2026-05-29 (GMT+7).

Why Go 1.22 as the floor, why goroutine + buffered-channel agents instead of a custom actor runtime, why tagged-struct union instead of Go generics for sum types, why CGo for FFI, and why the runtime package is a plain go module rather than a framework.

*Full research note content for MEP-54 note 02.*
