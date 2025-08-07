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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function primeFactors($n) {
  $i = 2;
  $factors = [];
  while ((($i * $i) <= $n)) {
  if ((($n % $i) == 0)) {
  $factors = _append($factors, $i);
  $n = (_intdiv($n, $i));
} else {
  $i = ($i + 1);
}
};
  if (($n > 1)) {
  $factors = _append($factors, $n);
}
  return $factors;
};
  function isSquareFree($factors) {
  $seen = [];
  foreach ($factors as $f) {
  if ((array_key_exists($f, $seen))) {
  return false;
}
  $seen[$f] = true;
};
  return true;
};
  function mobius($n) {
  $factors = primeFactors($n);
  if ((isSquareFree($factors))) {
  return (((fmod(count($factors), 2)) == 0) ? 1 : (-1));
}
  return 0;
};
  echo rtrim(json_encode(mobius(24), 1344)), PHP_EOL;
  echo rtrim(json_encode(mobius(-1), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
