# SPOJ COMEXP - Complex expression calculator

https://www.spoj.com/problems/COMEXP/

Evaluate arithmetic expression with +,-,*,/,%,^ (binary), unary -, abs(), sqrt(),
and postfix ! (factorial). Variables a-z with given values. Output 2 decimal places.

## Approach

Shunting-yard algorithm converts infix to RPN. Stack-based evaluation with values
scaled by 100 (fixed-point 2 decimal places). sqrt uses binary search: floor(sqrt(real)*100).
Bracket check first, then parse errors from shunting-yard, then evaluation errors.

## Notes

Uses isqrtScaled(val) = binary search for largest r where r^2 <= val*100.
Avoids int() taint bug by keeping values as list<int> and using loop-based sqrt.
