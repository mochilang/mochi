# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-27 22:11 +0700
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
class Line:
    slope: float
    yint: float

def createLine(a, b):
    slope = (b.y - a.y) / (b.x - a.x)
    yint = a.y - slope * a.x
    return Line(slope=slope, yint=yint)
def evalX(l, x):
    return l.slope * x + l.yint
def intersection(l1, l2):
    if l1.slope == l2.slope:
        return Point(x=0.0, y=0.0)
    x = (l2.yint - l1.yint) / (l1.slope - l2.slope)
    y = evalX(l1, x)
    return Point(x=x, y=y)
def main():
    l1 = createLine(Point(x=4.0, y=0.0), Point(x=6.0, y=10.0))
    l2 = createLine(Point(x=0.0, y=3.0), Point(x=10.0, y=7.0))
    p = intersection(l1, l2)
    print("{" + str(p.x) + " " + str(p.y) + "}")
main()
