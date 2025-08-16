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
$__start_mem = memory_get_usage();
$__start = _now();
  function is_luhn($s) {
  $n = strlen($s);
  if ($n <= 1) {
  return false;
}
  $check_digit = intval(substr($s, $n - 1, $n - ($n - 1)));
  $i = $n - 2;
  $even = true;
  while ($i >= 0) {
  $digit = intval(substr($s, $i, $i + 1 - $i));
  if ($even) {
  $doubled = $digit * 2;
  if ($doubled > 9) {
  $doubled = $doubled - 9;
};
  $check_digit = $check_digit + $doubled;
} else {
  $check_digit = $check_digit + $digit;
}
  $even = !$even;
  $i = $i - 1;
};
  return $check_digit % 10 == 0;
};
  echo str_replace('    ', '  ', json_encode(is_luhn('79927398713'), 128)), PHP_EOL;
  echo str_replace('    ', '  ', json_encode(is_luhn('79927398714'), 128)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
