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
  $PI = 3.141592653589793;
  function mochi_floor($x) {
  global $PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function mochi_pow($x, $n) {
  global $PI;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * $x;
  $i = $i + 1;
};
  return $result;
};
  function factorial($n) {
  global $PI;
  $result = 1.0;
  $i = 2;
  while ($i <= $n) {
  $result = $result * (floatval($i));
  $i = $i + 1;
};
  return $result;
};
  function maclaurin_sin($theta, $accuracy) {
  global $PI;
  $t = $theta;
  $div = mochi_floor($t / (2.0 * $PI));
  $t = $t - 2.0 * $div * $PI;
  $sum = 0.0;
  $r = 0;
  while ($r < $accuracy) {
  $power = 2 * $r + 1;
  $sign = ($r % 2 == 0 ? 1.0 : -1.0);
  $sum = $sum + $sign * mochi_pow($t, $power) / factorial($power);
  $r = $r + 1;
};
  return $sum;
};
  function maclaurin_cos($theta, $accuracy) {
  global $PI;
  $t = $theta;
  $div = mochi_floor($t / (2.0 * $PI));
  $t = $t - 2.0 * $div * $PI;
  $sum = 0.0;
  $r = 0;
  while ($r < $accuracy) {
  $power = 2 * $r;
  $sign = ($r % 2 == 0 ? 1.0 : -1.0);
  $sum = $sum + $sign * mochi_pow($t, $power) / factorial($power);
  $r = $r + 1;
};
  return $sum;
};
  echo rtrim(_str(maclaurin_sin(10.0, 30))), PHP_EOL;
  echo rtrim(_str(maclaurin_sin(-10.0, 30))), PHP_EOL;
  echo rtrim(_str(maclaurin_sin(10.0, 15))), PHP_EOL;
  echo rtrim(_str(maclaurin_sin(-10.0, 15))), PHP_EOL;
  echo rtrim(_str(maclaurin_cos(5.0, 30))), PHP_EOL;
  echo rtrim(_str(maclaurin_cos(-5.0, 30))), PHP_EOL;
  echo rtrim(_str(maclaurin_cos(10.0, 15))), PHP_EOL;
  echo rtrim(_str(maclaurin_cos(-10.0, 15))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
