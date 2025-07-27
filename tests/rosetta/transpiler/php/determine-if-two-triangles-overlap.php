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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fmt1($f) {
  $s = _str($f);
  $idx = _indexof($s, '.');
  if ($idx < 0) {
  $s = $s . '.0';
} else {
  $need = $idx + 2;
  if (strlen($s) > $need) {
  $s = substr($s, 0, $need - 0);
} else {
  while (strlen($s) < $need) {
  $s = $s . '0';
};
};
}
  return $s;
};
  function pointStr($p) {
  return '(' . fmt1($p['x']) . ', ' . fmt1($p['y']) . ')';
};
  function triangleStr($t) {
  return 'Triangle ' . pointStr($t['p1']) . ', ' . pointStr($t['p2']) . ', ' . pointStr($t['p3']);
};
  function orient($a, $b, $c) {
  return ($b['x'] - $a['x']) * ($c['y'] - $a['y']) - ($b['y'] - $a['y']) * ($c['x'] - $a['x']);
};
  function pointInTri($p, $t, $onBoundary) {
  $d1 = orient($p, $t['p1'], $t['p2']);
  $d2 = orient($p, $t['p2'], $t['p3']);
  $d3 = orient($p, $t['p3'], $t['p1']);
  $hasNeg = $d1 < 0.0 || $d2 < 0.0 || $d3 < 0.0;
  $hasPos = $d1 > 0.0 || $d2 > 0.0 || $d3 > 0.0;
  if ($onBoundary) {
  return !($hasNeg && $hasPos);
}
  return !($hasNeg && $hasPos) && $d1 != 0.0 && $d2 != 0.0 && $d3 != 0.0;
};
  function edgeCheck($a0, $a1, $bs, $onBoundary) {
  $d0 = orient($a0, $a1, $bs[0]);
  $d1 = orient($a0, $a1, $bs[1]);
  $d2 = orient($a0, $a1, $bs[2]);
  if ($onBoundary) {
  return $d0 <= 0.0 && $d1 <= 0.0 && $d2 <= 0.0;
}
  return $d0 < 0.0 && $d1 < 0.0 && $d2 < 0.0;
};
  function triTri2D($t1, $t2, $onBoundary) {
  $a = [$t1['p1'], $t1['p2'], $t1['p3']];
  $b = [$t2['p1'], $t2['p2'], $t2['p3']];
  $i = 0;
  while ($i < 3) {
  $j = ($i + 1) % 3;
  if (edgeCheck($a[$i], $a[$j], $b, $onBoundary)) {
  return false;
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < 3) {
  $j = ($i + 1) % 3;
  if (edgeCheck($b[$i], $b[$j], $a, $onBoundary)) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function iff($cond, $a, $b) {
  if ($cond) {
  return $a;
} else {
  return $b;
}
};
  function main() {
  $t1 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 5.0, 'y' => 0.0], 'p3' => ['x' => 0.0, 'y' => 5.0]];
  $t2 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 5.0, 'y' => 0.0], 'p3' => ['x' => 0.0, 'y' => 6.0]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t1 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 0.0, 'y' => 5.0], 'p3' => ['x' => 5.0, 'y' => 0.0]];
  $t2 = $t1;
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap (reversed)', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t1 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 5.0, 'y' => 0.0], 'p3' => ['x' => 0.0, 'y' => 5.0]];
  $t2 = ['p1' => ['x' => -10.0, 'y' => 0.0], 'p2' => ['x' => -5.0, 'y' => 0.0], 'p3' => ['x' => -1.0, 'y' => 6.0]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t1['p3'] = ['x' => 2.5, 'y' => 5.0];
  $t2 = ['p1' => ['x' => 0.0, 'y' => 4.0], 'p2' => ['x' => 2.5, 'y' => -1.0], 'p3' => ['x' => 5.0, 'y' => 4.0]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t1 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 1.0, 'y' => 1.0], 'p3' => ['x' => 0.0, 'y' => 2.0]];
  $t2 = ['p1' => ['x' => 2.0, 'y' => 1.0], 'p2' => ['x' => 3.0, 'y' => 0.0], 'p3' => ['x' => 3.0, 'y' => 2.0]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t2 = ['p1' => ['x' => 2.0, 'y' => 1.0], 'p2' => ['x' => 3.0, 'y' => -2.0], 'p3' => ['x' => 3.0, 'y' => 4.0]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $t1 = ['p1' => ['x' => 0.0, 'y' => 0.0], 'p2' => ['x' => 1.0, 'y' => 0.0], 'p3' => ['x' => 0.0, 'y' => 1.0]];
  $t2 = ['p1' => ['x' => 1.0, 'y' => 0.0], 'p2' => ['x' => 2.0, 'y' => 0.0], 'p3' => ['x' => 1.0, 'y' => 1.1]];
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  echo rtrim('which have only a single corner in contact, if boundary points collide'), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, true);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim(triangleStr($t1) . ' and'), PHP_EOL;
  echo rtrim(triangleStr($t2)), PHP_EOL;
  echo rtrim('which have only a single corner in contact, if boundary points do not collide'), PHP_EOL;
  $overlapping = triTri2D($t1, $t2, false);
  echo rtrim(iff($overlapping, 'overlap', 'do not overlap')), PHP_EOL;
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
