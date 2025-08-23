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
  $DIGITS = '0123456789';
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $LOOKUP_LETTERS = 'TRWAGMYFPDXBNJZSQVHLCKE';
  $ERROR_MSG = 'Input must be a string of 8 numbers plus letter';
  function to_upper($s) {
  global $DIGITS, $ERROR_MSG, $LOOKUP_LETTERS, $LOWER, $UPPER;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $converted = $ch;
  while ($j < strlen($LOWER)) {
  if (substr($LOWER, $j, $j + 1 - $j) == $ch) {
  $converted = substr($UPPER, $j, $j + 1 - $j);
  break;
}
  $j = $j + 1;
};
  $res = $res . $converted;
  $i = $i + 1;
};
  return $res;
};
  function is_digit($ch) {
  global $DIGITS, $ERROR_MSG, $LOOKUP_LETTERS, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function clean_id($spanish_id) {
  global $DIGITS, $ERROR_MSG, $LOOKUP_LETTERS, $LOWER, $UPPER;
  $upper_id = to_upper($spanish_id);
  $cleaned = '';
  $i = 0;
  while ($i < strlen($upper_id)) {
  $ch = substr($upper_id, $i, $i + 1 - $i);
  if ($ch != '-') {
  $cleaned = $cleaned . $ch;
}
  $i = $i + 1;
};
  return $cleaned;
};
  function is_spain_national_id($spanish_id) {
  global $DIGITS, $ERROR_MSG, $LOOKUP_LETTERS, $LOWER, $UPPER;
  $sid = clean_id($spanish_id);
  if (strlen($sid) != 9) {
  _panic($ERROR_MSG);
}
  $i = 0;
  while ($i < 8) {
  if (!is_digit(substr($sid, $i, $i + 1 - $i))) {
  _panic($ERROR_MSG);
}
  $i = $i + 1;
};
  $number = intval(substr($sid, 0, 8));
  $letter = substr($sid, 8, 8 + 1 - 8);
  if (is_digit($letter)) {
  _panic($ERROR_MSG);
}
  $expected = substr($LOOKUP_LETTERS, $number % 23, $number % 23 + 1 - $number % 23);
  return $letter == $expected;
};
  function main() {
  global $DIGITS, $ERROR_MSG, $LOOKUP_LETTERS, $LOWER, $UPPER;
  echo rtrim(json_encode(is_spain_national_id('12345678Z'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678z'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678x'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678I'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678-Z'), 1344)), PHP_EOL;
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
