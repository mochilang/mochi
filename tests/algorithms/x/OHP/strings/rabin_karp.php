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
  $alphabet_size = 256;
  $modulus = 1000003;
  function index_of_char($s, $ch) {
  global $alphabet_size, $modulus;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function mochi_ord($ch) {
  global $alphabet_size, $modulus;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $digits = '0123456789';
  $idx = index_of_char($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = index_of_char($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  $idx = index_of_char($digits, $ch);
  if ($idx >= 0) {
  return 48 + $idx;
}
  if ($ch == 'ü') {
  return 252;
}
  if ($ch == 'Ü') {
  return 220;
}
  if ($ch == ' ') {
  return 32;
}
  return 0;
};
  function rabin_karp($pattern, $text) {
  global $alphabet_size, $modulus;
  $p_len = strlen($pattern);
  $t_len = strlen($text);
  if ($p_len > $t_len) {
  return false;
}
  $p_hash = 0;
  $t_hash = 0;
  $modulus_power = 1;
  $i = 0;
  while ($i < $p_len) {
  $p_hash = fmod((mochi_ord(substr($pattern, $i, $i + 1 - $i)) + $p_hash * $alphabet_size), $modulus);
  $t_hash = fmod((mochi_ord(substr($text, $i, $i + 1 - $i)) + $t_hash * $alphabet_size), $modulus);
  if ($i != $p_len - 1) {
  $modulus_power = ($modulus_power * $alphabet_size) % $modulus;
}
  $i = $i + 1;
};
  $j = 0;
  while ($j <= $t_len - $p_len) {
  if ($t_hash == $p_hash && substr($text, $j, $j + $p_len - $j) == $pattern) {
  return true;
}
  if ($j == $t_len - $p_len) {
  $j = $j + 1;
  continue;
}
  $t_hash = fmod((($t_hash - mochi_ord(substr($text, $j, $j + 1 - $j)) * $modulus_power) * $alphabet_size + mochi_ord(substr($text, $j + $p_len, $j + $p_len + 1 - ($j + $p_len)))), $modulus);
  if ($t_hash < 0) {
  $t_hash = $t_hash + $modulus;
}
  $j = $j + 1;
};
  return false;
};
  function test_rabin_karp() {
  global $alphabet_size, $modulus;
  $pattern1 = 'abc1abc12';
  $text1 = 'alskfjaldsabc1abc1abc12k23adsfabcabc';
  $text2 = 'alskfjaldsk23adsfabcabc';
  if (!rabin_karp($pattern1, $text1) || rabin_karp($pattern1, $text2)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern2 = 'ABABX';
  $text3 = 'ABABZABABYABABX';
  if (!rabin_karp($pattern2, $text3)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern3 = 'AAAB';
  $text4 = 'ABAAAAAB';
  if (!rabin_karp($pattern3, $text4)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern4 = 'abcdabcy';
  $text5 = 'abcxabcdabxabcdabcdabcy';
  if (!rabin_karp($pattern4, $text5)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern5 = 'Lü';
  $text6 = 'Lüsai';
  if (!rabin_karp($pattern5, $text6)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern6 = 'Lue';
  if (rabin_karp($pattern6, $text6)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  echo rtrim('Success.'), PHP_EOL;
};
  test_rabin_karp();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
