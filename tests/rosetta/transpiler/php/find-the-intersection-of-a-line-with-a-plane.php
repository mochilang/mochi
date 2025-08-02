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
  function add($a, $b) {
  return ['x' => $a['x'] + $b['x'], 'y' => $a['y'] + $b['y'], 'z' => $a['z'] + $b['z']];
};
  function sub($a, $b) {
  return ['x' => $a['x'] - $b['x'], 'y' => $a['y'] - $b['y'], 'z' => $a['z'] - $b['z']];
};
  function mul($v, $s) {
  return ['x' => $v['x'] * $s, 'y' => $v['y'] * $s, 'z' => $v['z'] * $s];
};
  function dot($a, $b) {
  return $a['x'] * $b['x'] + $a['y'] * $b['y'] + $a['z'] * $b['z'];
};
  function intersectPoint($rv, $rp, $pn, $pp) {
  $diff = sub($rp, $pp);
  $prod1 = dot($diff, $pn);
  $prod2 = dot($rv, $pn);
  $prod3 = $prod1 / $prod2;
  return sub($rp, mul($rv, $prod3));
};
  function main() {
  $rv = ['x' => 0.0, 'y' => -1.0, 'z' => -1.0];
  $rp = ['x' => 0.0, 'y' => 0.0, 'z' => 10.0];
  $pn = ['x' => 0.0, 'y' => 0.0, 'z' => 1.0];
  $pp = ['x' => 0.0, 'y' => 0.0, 'z' => 5.0];
  $ip = intersectPoint($rv, $rp, $pn, $pp);
  echo rtrim('The ray intersects the plane at (' . _str($ip['x']) . ', ' . _str($ip['y']) . ', ' . _str($ip['z']) . ')'), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
