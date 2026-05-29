---
title: "MEP-56 note 08: Dataset pipeline"
sidebar_label: "08. Dataset pipeline"
sidebar_position: 9
description: "Mochi query DSL lowered to Ruby Enumerable::Lazy chains (select/map/take/drop/sort_by/group_by). Deterministic group_by ordering via sort_by. Compile-time Datalog semi-naive fixpoint emitted as frozen Array literals."
---

# MEP-56 note 08: Dataset pipeline

Author: research pass for MEP-56 (Mochi to Ruby transpiler).
Date: 2026-05-29 (GMT+7).

Mochi query DSL lowered to Ruby Enumerable::Lazy chains (select/map/take/drop/sort_by/group_by). Deterministic group_by ordering via sort_by. Compile-time Datalog semi-naive fixpoint emitted as frozen Array literals.

*Full research note content for MEP-56 note 08.*
