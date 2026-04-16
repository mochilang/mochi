# TJUC1

https://www.spoj.com/problems/TJUC1/

Given a and b, compute f(a)+f(a+1)+...+f(b) where f(x) = number of divisors of x. a,b up to 2^31-1. Input terminated by 0 0.

Uses D(n) = 2*sum_{k=1}^{isqrt(n)} floor(n/k) - isqrt(n)^2, then answer = D(b) - D(a-1). Integer square root via Newton's method.
