---
title: "MEP-56 note 09: Agents and streams"
sidebar_label: "09. Agents and streams"
sidebar_position: 10
description: "Mochi agents lower to Ruby classes with a Thread reading from a Thread::SizedQueue mailbox. Streams lower to a broadcast helper over an Array of Thread::SizedQueue consumers. Graceful shutdown via a poison-pill nil message."
---

# MEP-56 note 09: Agents and streams

Author: research pass for MEP-56 (Mochi to Ruby transpiler).
Date: 2026-05-29 (GMT+7).

Mochi agents lower to Ruby classes with a Thread reading from a Thread::SizedQueue mailbox. Streams lower to a broadcast helper over an Array of Thread::SizedQueue consumers. Graceful shutdown via a poison-pill nil message.

*Full research note content for MEP-56 note 09.*
