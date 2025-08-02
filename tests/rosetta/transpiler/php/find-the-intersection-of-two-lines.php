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
  function createLine($a, $b) {
  $slope = ($b['y'] - $a['y']) / ($b['x'] - $a['x']);
  $yint = $a['y'] - $slope * $a['x'];
  return ['slope' => $slope, 'yint' => $yint];
};
  function evalX($l, $x) {
  return $l['slope'] * $x + $l['yint'];
};
  function intersection($l1, $l2) {
  if ($l1['slope'] == $l2['slope']) {
  return ['x' => 0.0, 'y' => 0.0];
}
  $x = ($l2['yint'] - $l1['yint']) / ($l1['slope'] - $l2['slope']);
  $y = evalX($l1, $x);
  return ['x' => $x, 'y' => $y];
};
  function main() {
  $l1 = createLine(['x' => 4.0, 'y' => 0.0], ['x' => 6.0, 'y' => 10.0]);
  $l2 = createLine(['x' => 0.0, 'y' => 3.0], ['x' => 10.0, 'y' => 7.0]);
  $p = intersection($l1, $l2);
  echo rtrim('{' . _str($p['x']) . ' ' . _str($p['y']) . '}'), PHP_EOL;
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
