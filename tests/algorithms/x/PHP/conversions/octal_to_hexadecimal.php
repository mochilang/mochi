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
  function octal_to_hex($octal) {
  global $nums, $t, $num;
  $s = $octal;
  if (strlen($s) >= 2 && substr($s, 0, 0 + 1 - 0) == '0' && substr($s, 1, 1 + 1 - 1) == 'o') {
  $s = substr($s, 2, strlen($s) - 2);
}
  if (strlen($s) == 0) {
  $panic('Empty string was passed to the function');
}
  $j = 0;
  while ($j < strlen($s)) {
  $c = substr($s, $j, $j + 1 - $j);
  if ($c != '0' && $c != '1' && $c != '2' && $c != '3' && $c != '4' && $c != '5' && $c != '6' && $c != '7') {
  $panic('Not a Valid Octal Number');
}
  $j = $j + 1;
};
  $decimal = 0;
  $k = 0;
  while ($k < strlen($s)) {
  $d = ord(substr($s, $k, $k + 1 - $k));
  $decimal = $decimal * 8 + $d;
  $k = $k + 1;
};
  $hex_chars = '0123456789ABCDEF';
  if ($decimal == 0) {
  return '0x';
}
  $hex = '';
  while ($decimal > 0) {
  $idx = $decimal % 16;
  $hex = substr($hex_chars, $idx, $idx + 1 - $idx) . $hex;
  $decimal = _intdiv($decimal, 16);
};
  return '0x' . $hex;
};
  $nums = ['030', '100', '247', '235', '007'];
  $t = 0;
  while ($t < count($nums)) {
  $num = $nums[$t];
  echo rtrim(octal_to_hex($num)), PHP_EOL;
  $t = $t + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
