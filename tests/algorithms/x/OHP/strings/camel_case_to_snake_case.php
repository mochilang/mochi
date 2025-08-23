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
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $DIGITS = '0123456789';
  function is_lower($ch) {
  global $DIGITS, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($LOWER)) {
  if (substr($LOWER, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_upper($ch) {
  global $DIGITS, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($UPPER)) {
  if (substr($UPPER, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_digit($ch) {
  global $DIGITS, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_alpha($ch) {
  global $DIGITS, $LOWER, $UPPER;
  if (is_lower($ch)) {
  return true;
}
  if (is_upper($ch)) {
  return true;
}
  return false;
};
  function is_alnum($ch) {
  global $DIGITS, $LOWER, $UPPER;
  if (is_alpha($ch)) {
  return true;
}
  if (is_digit($ch)) {
  return true;
}
  return false;
};
  function to_lower($ch) {
  global $DIGITS, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($UPPER)) {
  if (substr($UPPER, $i, $i + 1 - $i) == $ch) {
  return substr($LOWER, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $ch;
};
  function camel_to_snake_case($input_str) {
  global $DIGITS, $LOWER, $UPPER;
  $snake_str = '';
  $i = 0;
  $prev_is_digit = false;
  $prev_is_alpha = false;
  while ($i < strlen($input_str)) {
  $ch = substr($input_str, $i, $i + 1 - $i);
  if (is_upper($ch)) {
  $snake_str = $snake_str . '_' . to_lower($ch);
} else {
  if ($prev_is_digit && is_lower($ch)) {
  $snake_str = $snake_str . '_' . $ch;
} else {
  if ($prev_is_alpha && is_digit($ch)) {
  $snake_str = $snake_str . '_' . $ch;
} else {
  if (!is_alnum($ch)) {
  $snake_str = $snake_str . '_';
} else {
  $snake_str = $snake_str . $ch;
};
};
};
}
  $prev_is_digit = is_digit($ch);
  $prev_is_alpha = is_alpha($ch);
  $i = $i + 1;
};
  if (strlen($snake_str) > 0 && substr($snake_str, 0, 0 + 1) == '_') {
  $snake_str = substr($snake_str, 1, strlen($snake_str) - 1);
}
  return $snake_str;
};
  function main() {
  global $DIGITS, $LOWER, $UPPER;
  echo rtrim(camel_to_snake_case('someRandomString')), PHP_EOL;
  echo rtrim(camel_to_snake_case('SomeRandomStr#ng')), PHP_EOL;
  echo rtrim(camel_to_snake_case('123SomeRandom123String123')), PHP_EOL;
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
