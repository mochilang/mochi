# `core/math`

This module reimplements many functions from Go's `math` package using pure Mochi code.
It avoids any FFI calls so it can run in constrained environments.

## Supported functions

- Pi
- E
- abs
- acos
- acosh
- asin
- asinh
- atan
- atan2
- atanh
- cbrt
- ceil
- copysign
- cos
- cosh
- dim
- exp
- exp2
- expm1
- fma
- floor
- hypot
- log
- log10
- log1p
- log2
- logb
- max
- min
- mod
- modf
- pow
- pow10
- frexp
- ldexp
- ilogb
- erf
- erfc
- erfcinv
- erfinv
- gamma
- lgamma
- nextafter
- nextafter32
- remainder
- round
- roundToEven
- signbit
- sin
- sincos
- sinh
- sqrt
- tan
- tanh
- trunc

## Unsupported functions (partial)

The following functions from Go's package are not yet implemented:

- Float32bits
- Float32frombits
- Float64bits
- Float64frombits
- Inf
- IsInf
- IsNaN
- J0
- J1
- Jn
- NaN
- Y0
- Y1
- Yn

