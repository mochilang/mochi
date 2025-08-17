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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_float($base, $exp) {
  $result = 1.0;
  $exponent = $exp;
  if ($exponent < 0) {
  $exponent = -$exponent;
  $i = 0;
  while ($i < $exponent) {
  $result = $result * $base;
  $i = $i + 1;
};
  return 1.0 / $result;
}
  $i = 0;
  while ($i < $exponent) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function sum_of_geometric_progression($first_term, $common_ratio, $num_of_terms) {
  if ($common_ratio == 1) {
  return floatval(($num_of_terms * $first_term));
}
  $a = floatval($first_term);
  $r = floatval($common_ratio);
  return ($a / (1.0 - $r)) * (1.0 - pow_float($r, $num_of_terms));
};
  function test_sum() {
  if (sum_of_geometric_progression(1, 2, 10) != 1023.0) {
  _panic('example1 failed');
}
  if (sum_of_geometric_progression(1, 10, 5) != 11111.0) {
  _panic('example2 failed');
}
  if (sum_of_geometric_progression(-1, 2, 10) != (-1023.0)) {
  _panic('example3 failed');
}
};
  function main() {
  test_sum();
  echo rtrim(json_encode(sum_of_geometric_progression(1, 2, 10), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
