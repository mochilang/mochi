<?php
ini_set('memory_limit', '-1');
function absf($x) {
  global $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function maxf($a, $b) {
  global $absf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  if ($a > $b) {
  return $a;
}
  return $b;
}
function minf($a, $b) {
  global $absf, $maxf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  if ($a < $b) {
  return $a;
}
  return $b;
}
function max3($a, $b, $c) {
  global $absf, $maxf, $minf, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  $m = $a;
  if ($b > $m) {
  $m = $b;
}
  if ($c > $m) {
  $m = $c;
}
  return $m;
}
function min3($a, $b, $c) {
  global $absf, $maxf, $minf, $max3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
}
function subdivideQuadSpline($q, $t) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  $s = 1.0 - $t;
  $u = ['c0' => $q['c0'], 'c1' => 0.0, 'c2' => 0.0];
  $v = ['c0' => 0.0, 'c1' => 0.0, 'c2' => $q['c2']];
  $u['c1'] = $s * $q['c0'] + $t * $q['c1'];
  $v['c1'] = $s * $q['c1'] + $t * $q['c2'];
  $u['c2'] = $s * $u['c1'] + $t * $v['c1'];
  $v['c0'] = $u['c2'];
  return [$u, $v];
}
function subdivideQuadCurve($q, $t) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  $xs = subdivideQuadSpline($q['x'], $t);
  $ys = subdivideQuadSpline($q['y'], $t);
  $u = ['x' => $xs[0], 'y' => $ys[0]];
  $v = ['x' => $xs[1], 'y' => $ys[1]];
  return [$u, $v];
}
function rectsOverlap($xa0, $ya0, $xa1, $ya1, $xb0, $yb0, $xb1, $yb1) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $testIntersect, $seemsToBeDuplicate, $findIntersects, $main;
  return $xb0 <= $xa1 && $xa0 <= $xb1 && $yb0 <= $ya1 && $ya0 <= $yb1;
}
function testIntersect($p, $q, $tol) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $seemsToBeDuplicate, $findIntersects, $main;
  $pxmin = min3($p['x']['c0'], $p['x']['c1'], $p['x']['c2']);
  $pymin = min3($p['y']['c0'], $p['y']['c1'], $p['y']['c2']);
  $pxmax = max3($p['x']['c0'], $p['x']['c1'], $p['x']['c2']);
  $pymax = max3($p['y']['c0'], $p['y']['c1'], $p['y']['c2']);
  $qxmin = min3($q['x']['c0'], $q['x']['c1'], $q['x']['c2']);
  $qymin = min3($q['y']['c0'], $q['y']['c1'], $q['y']['c2']);
  $qxmax = max3($q['x']['c0'], $q['x']['c1'], $q['x']['c2']);
  $qymax = max3($q['y']['c0'], $q['y']['c1'], $q['y']['c2']);
  $exclude = true;
  $accept = false;
  $inter = ['x' => 0.0, 'y' => 0.0];
  if (rectsOverlap($pxmin, $pymin, $pxmax, $pymax, $qxmin, $qymin, $qxmax, $qymax)) {
  $exclude = false;
  $xmin = maxf($pxmin, $qxmin);
  $xmax = minf($pxmax, $qxmax);
  if ($xmax - $xmin <= $tol) {
  $ymin = maxf($pymin, $qymin);
  $ymax = minf($pymax, $qymax);
  if ($ymax - $ymin <= $tol) {
  $accept = true;
  $inter['x'] = 0.5 * ($xmin + $xmax);
  $inter['y'] = 0.5 * ($ymin + $ymax);
};
};
}
  return ['exclude' => $exclude, 'accept' => $accept, 'intersect' => $inter];
}
function seemsToBeDuplicate($pts, $xy, $spacing) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $findIntersects, $main;
  $i = 0;
  while ($i < count($pts)) {
  $pt = $pts[$i];
  if (absf($pt['x'] - $xy['x']) < $spacing && absf($pt['y'] - $xy['y']) < $spacing) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function findIntersects($p, $q, $tol, $spacing) {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $main;
  $inters = [];
  $workload = [['p' => $p, 'q' => $q]];
  while (count($workload) > 0) {
  $idx = count($workload) - 1;
  $work = $workload[$idx];
  $workload = array_slice($workload, 0, $idx - 0);
  $res = testIntersect($work['p'], $work['q'], $tol);
  $excl = $res['exclude'];
  $acc = $res['accept'];
  $inter = $res['intersect'];
  if ($acc) {
  if (!seemsToBeDuplicate($inters, $inter, $spacing)) {
  $inters = array_merge($inters, [$inter]);
};
} else {
  if (!$excl) {
  $ps = subdivideQuadCurve($work['p'], 0.5);
  $qs = subdivideQuadCurve($work['q'], 0.5);
  $p0 = $ps[0];
  $p1 = $ps[1];
  $q0 = $qs[0];
  $q1 = $qs[1];
  $workload = array_merge($workload, [['p' => $p0, 'q' => $q0]]);
  $workload = array_merge($workload, [['p' => $p0, 'q' => $q1]]);
  $workload = array_merge($workload, [['p' => $p1, 'q' => $q0]]);
  $workload = array_merge($workload, [['p' => $p1, 'q' => $q1]]);
};
}
};
  return $inters;
}
function main() {
  global $absf, $maxf, $minf, $max3, $min3, $subdivideQuadSpline, $subdivideQuadCurve, $rectsOverlap, $testIntersect, $seemsToBeDuplicate, $findIntersects;
  $p = ['x' => ['c0' => -1.0, 'c1' => 0.0, 'c2' => 1.0], 'y' => ['c0' => 0.0, 'c1' => 10.0, 'c2' => 0.0]];
  $q = ['x' => ['c0' => 2.0, 'c1' => -8.0, 'c2' => 2.0], 'y' => ['c0' => 1.0, 'c1' => 2.0, 'c2' => 3.0]];
  $tol = 0.0000001;
  $spacing = $tol * 10.0;
  $inters = findIntersects($p, $q, $tol, $spacing);
  $i = 0;
  while ($i < count($inters)) {
  $pt = $inters[$i];
  echo rtrim('(' . json_encode($pt['x'], 1344) . ', ' . json_encode($pt['y'], 1344) . ')'), PHP_EOL;
  $i = $i + 1;
};
}
main();
