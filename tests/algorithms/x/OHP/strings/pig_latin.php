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
  $VOWELS = 'aeiou';
  function strip($s) {
  global $VOWELS;
  $start = 0;
  $end = strlen($s);
  while ($start < $end && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end > $start && substr($s, $end - 1, $end - ($end - 1)) == ' ') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function is_vowel($c) {
  global $VOWELS;
  $i = 0;
  while ($i < strlen($VOWELS)) {
  if ($c == substr($VOWELS, $i, $i + 1 - $i)) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function pig_latin($word) {
  global $VOWELS;
  $trimmed = strip($word);
  if (strlen($trimmed) == 0) {
  return '';
}
  $w = strtolower($trimmed);
  $first = substr($w, 0, 1);
  if (is_vowel($first)) {
  return $w . 'way';
}
  $i = 0;
  while ($i < strlen($w)) {
  $ch = substr($w, $i, $i + 1 - $i);
  if (is_vowel($ch)) {
  break;
}
  $i = $i + 1;
};
  return substr($w, $i, strlen($w) - $i) . substr($w, 0, $i) . 'ay';
};
  echo rtrim('pig_latin(\'friends\') = ' . pig_latin('friends')), PHP_EOL;
  echo rtrim('pig_latin(\'smile\') = ' . pig_latin('smile')), PHP_EOL;
  echo rtrim('pig_latin(\'eat\') = ' . pig_latin('eat')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
