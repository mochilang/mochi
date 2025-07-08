package kt

import (
    "mochi/compiler/x/kotlin"
    "mochi/types"
)

type Compiler = kotlin.Compiler

func New(env *types.Env, src string) *Compiler { return kotlin.New(env, src) }
