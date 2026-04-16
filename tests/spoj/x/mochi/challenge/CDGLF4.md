# Majority Party

**Slug:** `CDGLF4` | **Section:** challenge | **Rank:** 22739
**URL:** https://www.spoj.com/problems/CDGLF4/

N houses with party votes. M queries (l,r): determine if any party appears
more than floor((r-l+1)/2) times in houses l..r. Print "yes" or "no".
Uses Boyer-Moore majority vote + verification. O(N) per query.
