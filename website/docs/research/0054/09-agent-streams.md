---
title: "MEP-54 note 09: Agents and streams"
sidebar_label: "09. Agents and streams"
sidebar_position: 10
description: "Mochi agents lower to a Go struct plus a goroutine reading from a buffered chan Message mailbox. Streams lower to a fan-out broadcast helper over a slice of buffered channels. Context cancellation for clean shutdown."
---

# MEP-54 note 09: Agents and streams

Author: research pass for MEP-54 (Mochi to Go transpiler).
Date: 2026-05-29 (GMT+7).

Mochi agents lower to a Go struct plus a goroutine reading from a buffered chan Message mailbox. Streams lower to a fan-out broadcast helper over a slice of buffered channels. Context cancellation for clean shutdown.

*Full research note content for MEP-54 note 09.*
