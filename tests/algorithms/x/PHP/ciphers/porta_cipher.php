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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $BASE_TOP = 'ABCDEFGHIJKLM';
  $BASE_BOTTOM = 'NOPQRSTUVWXYZ';
  function to_upper($s) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $replaced = false;
  while ($j < strlen($LOWER)) {
  if (substr($LOWER, $j, $j + 1 - $j) == $ch) {
  $res = $res . substr($UPPER, $j, $j + 1 - $j);
  $replaced = true;
  break;
}
  $j = $j + 1;
};
  if (!$replaced) {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function char_index($c) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $i = 0;
  while ($i < strlen($UPPER)) {
  if (substr($UPPER, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function rotate_right($s, $k) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $n = strlen($s);
  $shift = $k % $n;
  return substr($s, $n - $shift, $n - ($n - $shift)) . substr($s, 0, $n - $shift - 0);
};
  function table_for($c) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $idx = char_index($c);
  $shift = _intdiv($idx, 2);
  $row1 = rotate_right($BASE_BOTTOM, $shift);
  $pair = [$BASE_TOP, $row1];
  return $pair;
};
  function generate_table($key) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $up = to_upper($key);
  $i = 0;
  $result = [];
  while ($i < strlen($up)) {
  $ch = substr($up, $i, $i + 1 - $i);
  $pair = table_for($ch);
  $result = _append($result, $pair);
  $i = $i + 1;
};
  return $result;
};
  function str_index($s, $ch) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  function get_position($table, $ch) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $row = 0;
  if (str_index($table[0], $ch) == 0 - 1) {
  $row = 1;
}
  $col = str_index($table[$row], $ch);
  return [$row, $col];
};
  function get_opponent($table, $ch) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $pos = get_position($table, $ch);
  $row = $pos[0];
  $col = $pos[1];
  if ($col == 0 - 1) {
  return $ch;
}
  if ($row == 1) {
  return substr($table[0], $col, $col + 1 - $col);
}
  return substr($table[1], $col, $col + 1 - $col);
};
  function encrypt($key, $words) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $table = generate_table($key);
  $up_words = to_upper($words);
  $cipher = '';
  $count = 0;
  $i = 0;
  while ($i < strlen($up_words)) {
  $ch = substr($up_words, $i, $i + 1 - $i);
  $cipher = $cipher . get_opponent($table[$count], $ch);
  $count = fmod(($count + 1), count($table));
  $i = $i + 1;
};
  return $cipher;
};
  function decrypt($key, $words) {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  $res = encrypt($key, $words);
  return $res;
};
  function main() {
  global $UPPER, $LOWER, $BASE_TOP, $BASE_BOTTOM;
  echo rtrim(encrypt('marvin', 'jessica')), PHP_EOL;
  echo rtrim(decrypt('marvin', 'QRACRWU')), PHP_EOL;
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
