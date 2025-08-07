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
  function abs_val($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function validate_point($p) {
  if (count($p) == 0) {
  $panic('Missing an input');
}
};
  function manhattan_distance($a, $b) {
  validate_point($a);
  validate_point($b);
  if (count($a) != count($b)) {
  $panic('Both points must be in the same n-dimensional space');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $total = $total + abs_val($a[$i] - $b[$i]);
  $i = $i + 1;
};
  return $total;
};
  function manhattan_distance_one_liner($a, $b) {
  return manhattan_distance($a, $b);
};
  echo rtrim(_str(manhattan_distance([1.0, 1.0], [2.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(manhattan_distance([1.5, 1.5], [2.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(manhattan_distance_one_liner([1.5, 1.5], [2.5, 2.0]))), PHP_EOL;
  echo rtrim(_str(manhattan_distance_one_liner([-3.0, -3.0, -3.0], [0.0, 0.0, 0.0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
