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
  function is_int_palindrome($num) {
  if ($num < 0) {
  return false;
}
  $n = $num;
  $rev = 0;
  while ($n > 0) {
  $rev = $rev * 10 + ($n % 10);
  $n = _intdiv($n, 10);
};
  return $rev == $num;
};
  function main() {
  echo rtrim(json_encode(is_int_palindrome(-121), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_int_palindrome(0), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_int_palindrome(10), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_int_palindrome(11), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_int_palindrome(101), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_int_palindrome(120), 1344)), PHP_EOL;
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
