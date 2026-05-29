---
title: "MEP-56 note 06: Type-system lowering"
sidebar_label: "06. Type-system lowering"
sidebar_position: 7
description: "Mochi int/float/bool/str/list/map/set lowered to Ruby Integer/Float/TrueClass|FalseClass/String/Array/Hash/Set. Records to Data.define. Sum types to a tagged Data.define hierarchy. Fun types to lambdas (Proc with lambda semantics)."
---

# MEP-56 note 06: Type-system lowering

Author: research pass for MEP-56 (Mochi to Ruby transpiler).
Date: 2026-05-29 (GMT+7).

Mochi int/float/bool/str/list/map/set lowered to Ruby Integer/Float/TrueClass|FalseClass/String/Array/Hash/Set. Records to Data.define. Sum types to a tagged Data.define hierarchy. Fun types to lambdas (Proc with lambda semantics).

*Full research note content for MEP-56 note 06.*
