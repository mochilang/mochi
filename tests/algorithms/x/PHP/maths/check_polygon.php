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
  function check_polygon($nums) {
  global $_;
  if (count($nums) < 2) {
  $error('Monogons and Digons are not polygons in the Euclidean space');
}
  $i = 0;
  while ($i < count($nums)) {
  if ($nums[$i] <= 0.0) {
  $error('All values must be greater than 0');
}
  $i = $i + 1;
};
  $total = 0.0;
  $max_side = 0.0;
  $i = 0;
  while ($i < count($nums)) {
  $v = $nums[$i];
  $total = $total + $v;
  if ($v > $max_side) {
  $max_side = $v;
}
  $i = $i + 1;
};
  return $max_side < ($total - $max_side);
};
  echo rtrim(_str(check_polygon([6.0, 10.0, 5.0]))), PHP_EOL;
  echo rtrim(_str(check_polygon([3.0, 7.0, 13.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(check_polygon([1.0, 4.3, 5.2, 12.2]))), PHP_EOL;
  $nums = [3.0, 7.0, 13.0, 2.0];
  $_ = check_polygon($nums);
  echo rtrim(_str($nums)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
