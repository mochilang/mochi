" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| absf maxf minf max3 m min3 subdivideQuadSpline s u v subdivideQuadCurve xs ys rectsOverlap testIntersect pxmin pymin pxmax pymax qxmin qymin qxmax qymax exclude accept inter xmin xmax ymin ymax seemsToBeDuplicate i pt findIntersects inters workload idx work res excl acc ps qs p0 p1 q0 q1 main p q tol spacing |
absf := [:x | ((x < 0)) ifTrue: [
-x.
] .
x. ].
maxf := [:a :b | ((a > b)) ifTrue: [
a.
] .
b. ].
minf := [:a :b | ((a < b)) ifTrue: [
a.
] .
b. ].
max3 := [:a :b :c | m := a.
((b > m)) ifTrue: [
m := b.
] .
((c > m)) ifTrue: [
m := c.
] .
m. ].
min3 := [:a :b :c | m := a.
((b < m)) ifTrue: [
m := b.
] .
((c < m)) ifTrue: [
m := c.
] .
m. ].
subdivideQuadSpline := [:q :t | s := (1 - t).
u := Dictionary from:{'c0'->q at: 'c0'. 'c1'->0. 'c2'->0}.
v := Dictionary from:{'c0'->0. 'c1'->0. 'c2'->q at: 'c2'}.
u at: 'c1' put: (((s * (q at: 'c0'))) + ((t * (q at: 'c1')))).
v at: 'c1' put: (((s * (q at: 'c1'))) + ((t * (q at: 'c2')))).
u at: 'c2' put: (((s * u.c1)) + ((t * v.c1))).
v at: 'c0' put: u.c2.
{u. v}. ].
subdivideQuadCurve := [:q :t | xs := subdivideQuadSpline value: q at: 'x' value: t.
ys := subdivideQuadSpline value: q at: 'y' value: t.
u := Dictionary from:{'x'->xs at: 0. 'y'->ys at: 0}.
v := Dictionary from:{'x'->xs at: 1. 'y'->ys at: 1}.
{u. v}. ].
rectsOverlap := [:xa0 :ya0 :xa1 :ya1 :xb0 :yb0 :xb1 :yb1 | (((((((xb0 <= xa1)) and: [((xa0 <= xb1))])) and: [((yb0 <= ya1))])) and: [((ya0 <= yb1))]). ].
testIntersect := [:p :q :tol | pxmin := min3 value: p at: 'x' at: 'c0' value: p at: 'x' at: 'c1' value: p at: 'x' at: 'c2'.
pymin := min3 value: p at: 'y' at: 'c0' value: p at: 'y' at: 'c1' value: p at: 'y' at: 'c2'.
pxmax := max3 value: p at: 'x' at: 'c0' value: p at: 'x' at: 'c1' value: p at: 'x' at: 'c2'.
pymax := max3 value: p at: 'y' at: 'c0' value: p at: 'y' at: 'c1' value: p at: 'y' at: 'c2'.
qxmin := min3 value: q at: 'x' at: 'c0' value: q at: 'x' at: 'c1' value: q at: 'x' at: 'c2'.
qymin := min3 value: q at: 'y' at: 'c0' value: q at: 'y' at: 'c1' value: q at: 'y' at: 'c2'.
qxmax := max3 value: q at: 'x' at: 'c0' value: q at: 'x' at: 'c1' value: q at: 'x' at: 'c2'.
qymax := max3 value: q at: 'y' at: 'c0' value: q at: 'y' at: 'c1' value: q at: 'y' at: 'c2'.
exclude := true.
accept := false.
inter := Dictionary from:{'x'->0. 'y'->0}.
(rectsOverlap value: pxmin value: pymin value: pxmax value: pymax value: qxmin value: qymin value: qxmax value: qymax) ifTrue: [
exclude := false.
xmin := maxf value: pxmin value: qxmin.
xmax := minf value: pxmax value: qxmax.
((((xmax - xmin)) <= tol)) ifTrue: [
ymin := maxf value: pymin value: qymin.
ymax := minf value: pymax value: qymax.
((((ymax - ymin)) <= tol)) ifTrue: [
accept := true.
inter at: 'x' put: (0.5 * ((xmin + xmax))).
inter at: 'y' put: (0.5 * ((ymin + ymax))).
] .
] .
] .
Dictionary from:{'exclude'->exclude. 'accept'->accept. 'intersect'->inter}. ].
seemsToBeDuplicate := [:pts :xy :spacing | i := 0.
[(i < ((pts size)))] whileTrue: [.
pt := pts at: i.
(((((absf value: (pt.x - (xy at: 'x'))) < spacing)) and: [(((absf value: (pt.y - (xy at: 'y'))) < spacing))])) ifTrue: [
true.
] .
i := (i + 1).
].
.
false. ].
findIntersects := [:p :q :tol :spacing | inters := {}.
workload := {Dictionary from:{'p'->p. 'q'->q}}.
[(1 > 0)] whileTrue: [.
idx := (1 - 1).
work := workload at: idx.
workload := workload copyFrom: 1 to: idx.
res := testIntersect value: work at: 'p' value: work at: 'q' value: tol.
excl := res at: 'exclude'.
acc := res at: 'accept'.
inter := res at: 'intersect'.
(acc) ifTrue: [
(seemsToBeDuplicate value: inters value: inter value: spacing not) ifTrue: [
inters := inters copyWith: inter.
] .
] ifFalse: [
(excl not) ifTrue: [
ps := subdivideQuadCurve value: work at: 'p' value: 0.5.
qs := subdivideQuadCurve value: work at: 'q' value: 0.5.
p0 := ps at: 0.
p1 := ps at: 1.
q0 := qs at: 0.
q1 := qs at: 1.
workload := workload copyWith: Dictionary from:{'p'->p0. 'q'->q0}.
workload := workload copyWith: Dictionary from:{'p'->p0. 'q'->q1}.
workload := workload copyWith: Dictionary from:{'p'->p1. 'q'->q0}.
workload := workload copyWith: Dictionary from:{'p'->p1. 'q'->q1}.
] .
].
].
.
inters. ].
main := [ p := Dictionary from:{'x'->Dictionary from:{'c0'->-1. 'c1'->0. 'c2'->1}. 'y'->Dictionary from:{'c0'->0. 'c1'->10. 'c2'->0}}.
q := Dictionary from:{'x'->Dictionary from:{'c0'->2. 'c1'->-8. 'c2'->2}. 'y'->Dictionary from:{'c0'->1. 'c1'->2. 'c2'->3}}.
tol := 1e-07.
spacing := (tol * 10).
inters := findIntersects value: p value: q value: tol value: spacing.
i := 0.
[(i < ((inters size)))] whileTrue: [.
pt := inters at: i.
Transcript show: (((((((('(' + ((pt.x asString)))) + (', '))) + ((pt.y asString)))) + ')')) printString; cr.
i := (i + 1).
].
. ].
main value.
