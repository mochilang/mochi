<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function bead_sort($sequence) {
  $n = count($sequence);
  $i = 0;
  while ($i < $n) {
  if ($sequence[$i] < 0) {
  _panic('Sequence must be list of non-negative integers');
}
  $i = $i + 1;
};
  $pass = 0;
  while ($pass < $n) {
  $j = 0;
  while ($j < $n - 1) {
  $upper = $sequence[$j];
  $lower = $sequence[$j + 1];
  if ($upper > $lower) {
  $diff = $upper - $lower;
  $sequence[$j] = $upper - $diff;
  $sequence[$j + 1] = $lower + $diff;
}
  $j = $j + 1;
};
  $pass = $pass + 1;
};
  return $sequence;
};
  echo rtrim(_str(bead_sort([6, 11, 12, 4, 1, 5]))), PHP_EOL;
  echo rtrim(_str(bead_sort([9, 8, 7, 6, 5, 4, 3, 2, 1]))), PHP_EOL;
  echo rtrim(_str(bead_sort([5, 0, 4, 3]))), PHP_EOL;
  echo rtrim(_str(bead_sort([8, 2, 1]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
