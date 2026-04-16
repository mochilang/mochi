**WOW_SQR - Wow Square**
URL: https://www.spoj.com/problems/WOW_SQR/

Find x < y <= n maximizing x*y as a perfect square (tiebreak: max y-x).
Key insight: x*y is a perfect square iff x=s*a², y=s*b² for squarefree s, a<b.
Iterate b from 2 to floor(sqrt(n)), set s=floor(n/b²), a=b-1, compute x=s*(b-1)², y=s*b².
Score = source length.
