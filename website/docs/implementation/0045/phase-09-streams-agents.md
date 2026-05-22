---
title: "Phase 9. Streams, agents, M:N scheduler"
sidebar_position: 11
sidebar_label: "Phase 9. Streams + agents"
description: "MEP-45 Phase 9 tracking: M:N work-stealing scheduler over minicoro, bounded channels, broadcast streams, agent mailboxes, graceful shutdown."
---

# Phase 9. Streams, agents, M:N scheduler

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 9](/docs/mep/mep-0045#phase-9-streams-agents-m-scheduler) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Streams + agents fixture suite (~40 cases: stream emit/subscribe, agent intent dispatch, bounded channel back-pressure, shutdown, fan-out fan-in) compiles + runs byte-equal vs vm3 on host triple under TSan-clean execution.

## Goal-alignment audit

_To be written before sub-phase 9.0 starts. Streams + agents differentiate Mochi from other small languages; agent-style programs need this surface to compile. Aligns._

## Sub-phases

| #   | Scope                                                                                                              | Status      | Commit | PR |
|-----|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 9.0 | M:N work-stealing scheduler over minicoro; one OS thread per hardware core; blocking syscalls on overflow pool     | NOT STARTED | —      | — |
| 9.1 | `chan<T>`: bounded ring, point-to-point, send blocks when full                                                     | NOT STARTED | —      | — |
| 9.2 | `stream<T>`: bounded ring + subscriber list (MPMC broadcast); `emit` blocks when any subscriber full               | NOT STARTED | —      | — |
| 9.3 | Agent: record with embedded mailbox; intent calls enqueue typed messages; run loop on dedicated fiber              | NOT STARTED | —      | — |
| 9.4 | Shutdown protocol: graceful drain on SIGINT/SIGTERM; bounded-time hard kill after timeout                          | NOT STARTED | —      | — |

## Decisions made

_Fill in along the way._

## Deferred work

_CPU preemption (Go-style signal preemption): v2._

## Closeout notes

_Fill in after gate green._
