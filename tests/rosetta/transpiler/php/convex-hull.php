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
  function ccw($a, $b, $c) {
  global $pts, $hull;
  $lhs = ($b['x'] - $a['x']) * ($c['y'] - $a['y']);
  $rhs = ($b['y'] - $a['y']) * ($c['x'] - $a['x']);
  return $lhs > $rhs;
};
  function sortPoints($ps) {
  global $pts, $hull;
  $arr = $ps;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  $p = $arr[$j];
  $q = $arr[$j + 1];
  if ($p['x'] > $q['x'] || ($p['x'] == $q['x'] && $p['y'] > $q['y'])) {
  $arr[$j] = $q;
  $arr[$j + 1] = $p;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function convexHull($ps) {
  global $pts, $hull;
  $ps = sortPoints($ps);
  $h = [];
  foreach ($ps as $pt) {
  while (count($h) >= 2 && ccw($h[count($h) - 2], $h[count($h) - 1], $pt) == false) {
  $h = array_slice($h, 0, count($h) - 1 - 0);
};
  $h = array_merge($h, [$pt]);
};
  $i = count($ps) - 2;
  $t = count($h) + 1;
  while ($i >= 0) {
  $pt = $ps[$i];
  while (count($h) >= $t && ccw($h[count($h) - 2], $h[count($h) - 1], $pt) == false) {
  $h = array_slice($h, 0, count($h) - 1 - 0);
};
  $h = array_merge($h, [$pt]);
  $i = $i - 1;
};
  return array_slice($h, 0, count($h) - 1 - 0);
};
  function pointStr($p) {
  global $pts, $hull;
  return '(' . _str($p['x']) . ',' . _str($p['y']) . ')';
};
  function hullStr($h) {
  global $pts, $hull;
  $s = '[';
  $i = 0;
  while ($i < count($h)) {
  $s = $s . pointStr($h[$i]);
  if ($i < count($h) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $pts = [['x' => 16, 'y' => 3], ['x' => 12, 'y' => 17], ['x' => 0, 'y' => 6], ['x' => -4, 'y' => -6], ['x' => 16, 'y' => 6], ['x' => 16, 'y' => -7], ['x' => 16, 'y' => -3], ['x' => 17, 'y' => -4], ['x' => 5, 'y' => 19], ['x' => 19, 'y' => -8], ['x' => 3, 'y' => 16], ['x' => 12, 'y' => 13], ['x' => 3, 'y' => -4], ['x' => 17, 'y' => 5], ['x' => -3, 'y' => 15], ['x' => -3, 'y' => -9], ['x' => 0, 'y' => 11], ['x' => -9, 'y' => -3], ['x' => -4, 'y' => -2], ['x' => 12, 'y' => 10]];
  $hull = convexHull($pts);
  echo rtrim('Convex Hull: ' . hullStr($hull)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
