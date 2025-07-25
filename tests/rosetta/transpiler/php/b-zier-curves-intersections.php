<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function maxf($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function minf($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function max3($a, $b, $c) {
  $m = $a;
  if ($b > $m) {
  $m = $b;
}
  if ($c > $m) {
  $m = $c;
}
  return $m;
};
  function min3($a, $b, $c) {
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
};
  function subdivideQuadSpline($q, $t) {
  $s = 1.0 - $t;
  $u = ['c0' => $q['c0'], 'c1' => 0.0, 'c2' => 0.0];
  $v = ['c0' => 0.0, 'c1' => 0.0, 'c2' => $q['c2']];
  $u['c1'] = $s * $q['c0'] + $t * $q['c1'];
  $v['c1'] = $s * $q['c1'] + $t * $q['c2'];
  $u['c2'] = $s * $u['c1'] + $t * $v['c1'];
  $v['c0'] = $u['c2'];
  return [$u, $v];
};
  function subdivideQuadCurve($q, $t) {
  $xs = subdivideQuadSpline($q['x'], $t);
  $ys = subdivideQuadSpline($q['y'], $t);
  $u = ['x' => $xs[0], 'y' => $ys[0]];
  $v = ['x' => $xs[1], 'y' => $ys[1]];
  return [$u, $v];
};
  function rectsOverlap($xa0, $ya0, $xa1, $ya1, $xb0, $yb0, $xb1, $yb1) {
  return $xb0 <= $xa1 && $xa0 <= $xb1 && $yb0 <= $ya1 && $ya0 <= $yb1;
};
  function testIntersect($p, $q, $tol) {
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
};
  function seemsToBeDuplicate($pts, $xy, $spacing) {
  $i = 0;
  while ($i < count($pts)) {
  $pt = $pts[$i];
  if (absf($pt['x'] - $xy['x']) < $spacing && absf($pt['y'] - $xy['y']) < $spacing) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function findIntersects($p, $q, $tol, $spacing) {
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
};
  function main() {
  $p = ['x' => ['c0' => -1.0, 'c1' => 0.0, 'c2' => 1.0], 'y' => ['c0' => 0.0, 'c1' => 10.0, 'c2' => 0.0]];
  $q = ['x' => ['c0' => 2.0, 'c1' => -8.0, 'c2' => 2.0], 'y' => ['c0' => 1.0, 'c1' => 2.0, 'c2' => 3.0]];
  $tol = 0.0000001;
  $spacing = $tol * 10.0;
  $inters = findIntersects($p, $q, $tol, $spacing);
  $i = 0;
  while ($i < count($inters)) {
  $pt = $inters[$i];
  echo rtrim('(' . _str($pt['x']) . ', ' . _str($pt['y']) . ')'), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
