/- Solution for SPOJ CUBES - Perfect Cubes
https://www.spoj.com/problems/CUBES/
-/

import Std
open Std

def main : IO Unit := do
  for a in [2:101] do
    let cube := a * a * a
    for b in [2:a] do
      let b3 := b * b * b
      for c in [b:a] do
        let c3 := c * c * c
        for d in [c:a] do
          let sum := b3 + c3 + d * d * d
          if cube == sum then
            IO.println s!"Cube = {a}, Triple = ({b},{c},{d})"
