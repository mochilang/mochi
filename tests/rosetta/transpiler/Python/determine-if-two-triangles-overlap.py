# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 19:01 +0700
from __future__ import annotations
from dataclasses import dataclass
from typing import List, Dict
import dataclasses

import sys
sys.set_int_max_str_digits(0)

@dataclass
class Point:
    x: float
    y: float

@dataclass
class Triangle:
    p1: Point
    p2: Point
    p3: Point

def fmt1(f):
    s = str(f)
    idx = s.find(".")
    if idx < 0:
        s = s + ".0"
    else:
        need = idx + 2
        if len(s) > need:
            s = s[0:need]
        else:
            while len(s) < need:
                s = s + "0"
    return s
def pointStr(p):
    return "(" + fmt1(p.x) + ", " + fmt1(p.y) + ")"
def triangleStr(t):
    return "Triangle " + pointStr(t.p1) + ", " + pointStr(t.p2) + ", " + pointStr(t.p3)
def orient(a, b, c):
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
def pointInTri(p, t, onBoundary):
    d1 = orient(p, t.p1, t.p2)
    d2 = orient(p, t.p2, t.p3)
    d3 = orient(p, t.p3, t.p1)
    hasNeg = d1 < 0.0 or d2 < 0.0 or d3 < 0.0
    hasPos = d1 > 0.0 or d2 > 0.0 or d3 > 0.0
    if onBoundary:
        return not (hasNeg and hasPos)
    return not (hasNeg and hasPos) and d1 != 0.0 and d2 != 0.0 and d3 != 0.0
def edgeCheck(a0, a1, bs, onBoundary):
    d0 = orient(a0, a1, bs[0])
    d1 = orient(a0, a1, bs[1])
    d2 = orient(a0, a1, bs[2])
    if onBoundary:
        return d0 <= 0.0 and d1 <= 0.0 and d2 <= 0.0
    return d0 < 0.0 and d1 < 0.0 and d2 < 0.0
def triTri2D(t1, t2, onBoundary):
    a = [t1.p1, t1.p2, t1.p3]
    b = [t2.p1, t2.p2, t2.p3]
    i = 0
    while i < 3:
        j = (i + 1) % 3
        if edgeCheck(a[i], a[j], b, onBoundary):
            return False
        i = i + 1
    i = 0
    while i < 3:
        j = (i + 1) % 3
        if edgeCheck(b[i], b[j], a, onBoundary):
            return False
        i = i + 1
    return True
def iff(cond, a, b):
    if cond:
        return a
    else:
        return b
def main():
    t1 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=5.0, y=0.0), p3=Point(x=0.0, y=5.0))
    t2 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=5.0, y=0.0), p3=Point(x=0.0, y=6.0))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    t1 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=0.0, y=5.0), p3=Point(x=5.0, y=0.0))
    t2 = t1
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap (reversed)", "do not overlap"))
    print("")
    t1 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=5.0, y=0.0), p3=Point(x=0.0, y=5.0))
    t2 = Triangle(p1=Point(x=-10.0, y=0.0), p2=Point(x=-5.0, y=0.0), p3=Point(x=-1.0, y=6.0))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    t1 = dataclasses.replace(t1, p3=Point(x=2.5, y=5.0))
    t2 = Triangle(p1=Point(x=0.0, y=4.0), p2=Point(x=2.5, y=-1.0), p3=Point(x=5.0, y=4.0))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    t1 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=1.0, y=1.0), p3=Point(x=0.0, y=2.0))
    t2 = Triangle(p1=Point(x=2.0, y=1.0), p2=Point(x=3.0, y=0.0), p3=Point(x=3.0, y=2.0))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    t2 = Triangle(p1=Point(x=2.0, y=1.0), p2=Point(x=3.0, y=-2.0), p3=Point(x=3.0, y=4.0))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    t1 = Triangle(p1=Point(x=0.0, y=0.0), p2=Point(x=1.0, y=0.0), p3=Point(x=0.0, y=1.0))
    t2 = Triangle(p1=Point(x=1.0, y=0.0), p2=Point(x=2.0, y=0.0), p3=Point(x=1.0, y=1.1))
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    print("which have only a single corner in contact, if boundary points collide")
    overlapping = triTri2D(t1, t2, True)
    print(iff(overlapping, "overlap", "do not overlap"))
    print("")
    print(triangleStr(t1) + " and")
    print(triangleStr(t2))
    print("which have only a single corner in contact, if boundary points do not collide")
    overlapping = triTri2D(t1, t2, False)
    print(iff(overlapping, "overlap", "do not overlap"))
main()
