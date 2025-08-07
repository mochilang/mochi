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
  function is_arithmetic_series($xs) {
  if (count($xs) == 0) {
  $panic('Input list must be a non empty list');
}
  if (count($xs) == 1) {
  return true;
}
  $diff = $xs[1] - $xs[0];
  $i = 0;
  while ($i < count($xs) - 1) {
  if ($xs[$i + 1] - $xs[$i] != $diff) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function arithmetic_mean($xs) {
  if (count($xs) == 0) {
  $panic('Input list must be a non empty list');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($xs)) {
  $total = $total + $xs[$i];
  $i = $i + 1;
};
  return $total / (floatval(count($xs)));
};
  echo rtrim(_str(is_arithmetic_series([2.0, 4.0, 6.0]))), PHP_EOL;
  echo rtrim(_str(is_arithmetic_series([3.0, 6.0, 12.0, 24.0]))), PHP_EOL;
  echo rtrim(_str(arithmetic_mean([2.0, 4.0, 6.0]))), PHP_EOL;
  echo rtrim(_str(arithmetic_mean([3.0, 6.0, 9.0, 12.0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
