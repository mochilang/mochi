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
  function strip_spaces($s) {
  $start = 0;
  $end = strlen($s) - 1;
  while ($start < strlen($s) && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end >= $start && substr($s, $end, $end + 1 - $end) == ' ') {
  $end = $end - 1;
};
  $res = '';
  $i = $start;
  while ($i <= $end) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
};
  return $res;
};
  function repeat_char($ch, $count) {
  $res = '';
  $i = 0;
  while ($i < $count) {
  $res = $res . $ch;
  $i = $i + 1;
};
  return $res;
};
  function slice($s, $start, $end) {
  $res = '';
  $i = $start;
  while ($i < $end) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
};
  return $res;
};
  function bits_to_int($bits) {
  $value = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  $value = $value * 2;
  if (substr($bits, $i, $i + 1 - $i) == '1') {
  $value = $value + 1;
}
  $i = $i + 1;
};
  return $value;
};
  function bin_to_hexadecimal($binary_str) {
  $s = strip_spaces($binary_str);
  if (strlen($s) == 0) {
  $panic('Empty string was passed to the function');
}
  $is_negative = false;
  if (substr($s, 0, 0 + 1 - 0) == '-') {
  $is_negative = true;
  $s = array_slice($s, 1, strlen($s) - 1);
}
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c != '0' && $c != '1') {
  $panic('Non-binary value was passed to the function');
}
  $i = $i + 1;
};
  $groups = strlen($s) / 4 + 1;
  $pad_len = $groups * 4 - strlen($s);
  $s = repeat_char('0', $pad_len) . $s;
  $digits = '0123456789abcdef';
  $res = '0x';
  $j = 0;
  while ($j < strlen($s)) {
  $chunk = array_slice($s, $j, $j + 4 - $j);
  $val = bits_to_int($chunk);
  $res = $res . substr($digits, $val, $val + 1 - $val);
  $j = $j + 4;
};
  if ($is_negative) {
  return '-' . $res;
}
  return $res;
};
  echo rtrim(bin_to_hexadecimal('101011111')), PHP_EOL;
  echo rtrim(bin_to_hexadecimal(' 1010   ')), PHP_EOL;
  echo rtrim(bin_to_hexadecimal('-11101')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
