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
$__start_mem = memory_get_usage();
$__start = _now();
  function is_prime($n) {
  if ($n < 2) {
  return false;
}
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function solution($limit) {
  if ($limit < 2) {
  return 1;
}
  $result = 1;
  $p = 2;
  while ($result * $p <= $limit) {
  if (is_prime($p)) {
  $result = $result * $p;
}
  $p = $p + 1;
};
  return $result;
};
  function main() {
  $ans = solution(1000000);
  echo rtrim(json_encode($ans, 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
