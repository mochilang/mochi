# SPOJ ART - Artisticity

https://www.spoj.com/problems/ART/

Pack k artistic themes into a minimal painting (p*q <= Xi*Xi*k). Themes can appear
rotated by 0/90/180/270 degrees.

## Approach

Simple baseline: stack all themes vertically in original orientation.
Painting: height=sum(ni), width=max(mi). Each theme placed at rows [offset, offset+ni),
columns [0, mi), remainder padded with 'a'. Always satisfies p*q <= k*Xi*Xi.
Score = p*q/(Xi*Xi*k) >= 1/Xi (not optimal but always correct).
