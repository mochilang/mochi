/- Solution for SPOJ WA - Problems Collection (Volume X)
https://www.spoj.com/problems/WA/
-/
import Std
open Std

/-- Problem 1 --/
def problem1 : Int :=
  let base : Int := (10 - 1) * (10 - 2) * (10 - 3) * (10 - 4)
  let diff := base * (10 - (-5))
  diff + 2007 * (10 - (-5))

/-- Problem 2 --/
def problem2 : Int :=
  let big : Int := 2007 - (100 - 1)
  big * big + (100 - 1)

/-- Problem 3 --/
def problem3 : Int :=
  let mod := 126
  let step (acc : Nat) (c : Char) :=
    (acc * 10 + (c.toNat - '0'.toNat)) % mod
  let rec loop (n : Nat) (acc : Nat) : Nat :=
    if n > 500 then acc
    else loop (n + 1) ((toString n).foldl step acc)
  Int.ofNat (loop 100 0)

/-- Problem 4 --/
def problem4 : Int := 2007 * 2007

/-- Problem 5 --/
def problem5 : Int :=
  let bound := 200
  let rec loopN (n acc : Nat) : Nat :=
    if n > bound then acc
    else
      let rec loopM (m : Nat) : Bool :=
        if m > 2 * n then false
        else
          let lhs := Nat.factorial (m + n - 4)
          let rhs := Nat.factorial n * Nat.factorial (m - 1)
          if lhs = rhs then true else loopM (m + 1)
      let acc := if loopM 1 then acc + n else acc
      loopN (n + 1) acc
  Int.ofNat (loopN 4 0)

/-- Problem 6 --/
def problem6 : Int :=
  let u1 : Int := 3 * (1 - 17) / 2
  let v1 : Int := u1 / 3
  let area1 := 2 * (u1 * u1 + v1 * v1)
  let u2 : Int := 3 * (1 - 5) / 2
  let v2 : Int := u2 / 3
  let area2 := 2 * (u2 * u2 + v2 * v2)
  area1 + area2

/-- Problem 7 --/
def problem7 : Int := 3

/-- Fibonacci numbers --/
def fib : Nat → Int
| 0 => 0
| 1 => 1
| n+2 => fib (n+1) + fib n

/-- Problem 8 --/
def problem8 : Int :=
  let f15 := fib 15
  let f16 := fib 16
  let f17 := fib 17
  let a1 : Int := f16
  let a2 : Int := -f17
  a1 * a2

/-- Problem 9 --/
def problem9 : Int := 4

/-- Problem 10 --/
def problem10 : Int :=
  let x_num := (1 : Nat)
  let x_den := (5 : Nat)
  let y_num := (2 : Nat)
  let y_den := (3 : Nat)
  let z_num := (1 : Nat)
  let z_den := (1 : Nat)
  let m_num := x_num^4 * y_den^4 * z_den^4 +
                y_num^4 * x_den^4 * z_den^4 +
                z_num^4 * x_den^4 * y_den^4
  let m_den := x_den^4 * y_den^4 * z_den^4
  let g := Nat.gcd m_num m_den
  let m := m_num / g
  let n := m_den / g
  Int.ofNat (m + n)

/-- Main: output all answers line by line --/
def main : IO Unit := do
  let answers : List Int :=
    [ problem1, problem2, problem3, problem4, problem5,
      problem6, problem7, problem8, problem9, problem10 ]
  IO.println (String.intercalate "\n" (answers.map toString))
