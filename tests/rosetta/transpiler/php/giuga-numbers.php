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
  $factors = [];
  $last = 0;
  $x = $n;
  while ($x % 2 == 0) {
  if ($last == 2) {
  return [];
}
  $factors = array_merge($factors, [2]);
  $last = 2;
  $x = _intdiv($x, 2);
};
  $p = 3;
  while ($p * $p <= $x) {
  while ($x % $p == 0) {
  if ($last == $p) {
  return [];
}
  $factors = array_merge($factors, [$p]);
  $last = $p;
  $x = _intdiv($x, $p);
};
  $p = $p + 2;
};
  if ($x > 1) {
  if ($last == $x) {
  return [];
};
  $factors = array_merge($factors, [$x]);
}
  return $factors;
};
  function isGiuga($n) {
  $facs = primeFactors($n);
  if (count($facs) <= 2) {
  return false;
}
  foreach ($facs as $f) {
  if (((_intdiv($n, $f) - 1) % $f) != 0) {
  return false;
}
};
  return true;
};
  function main() {
  $known = [30, 858, 1722, 66198];
  $nums = [];
  foreach ($known as $n) {
  if (isGiuga($n)) {
  $nums = array_merge($nums, [$n]);
}
};
  echo rtrim('The first 4 Giuga numbers are:'), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($nums, 1344))))))), PHP_EOL;
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
