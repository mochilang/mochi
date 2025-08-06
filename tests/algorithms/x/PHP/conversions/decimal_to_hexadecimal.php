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
  $values = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
  function decimal_to_hexadecimal($decimal) {
  global $values;
  $num = $decimal;
  $negative = false;
  if ($num < 0) {
  $negative = true;
  $num = -$num;
}
  if ($num == 0) {
  if ($negative) {
  return '-0x0';
};
  return '0x0';
}
  $hex = '';
  while ($num > 0) {
  $remainder = $num % 16;
  $hex = $values[$remainder] . $hex;
  $num = _intdiv($num, 16);
};
  if ($negative) {
  return '-0x' . $hex;
}
  return '0x' . $hex;
};
  echo rtrim(decimal_to_hexadecimal(5)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(15)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(37)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(255)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(4096)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(999098)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(-256)), PHP_EOL;
  echo rtrim(decimal_to_hexadecimal(0)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
