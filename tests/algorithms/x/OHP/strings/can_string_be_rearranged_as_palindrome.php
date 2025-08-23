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
  $LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LOWERCASE = 'abcdefghijklmnopqrstuvwxyz';
  function char_to_lower($c) {
  global $LETTERS, $LOWERCASE;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if ($c == substr($LETTERS, $i, $i + 1 - $i)) {
  return substr($LOWERCASE, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $c;
};
  function normalize($input_str) {
  global $LETTERS, $LOWERCASE;
  $res = '';
  $i = 0;
  while ($i < strlen($input_str)) {
  $ch = substr($input_str, $i, $i + 1 - $i);
  $lc = char_to_lower($ch);
  if ($lc >= 'a' && $lc <= 'z') {
  $res = $res . $lc;
}
  $i = $i + 1;
};
  return $res;
};
  function can_string_be_rearranged_as_palindrome_counter($input_str) {
  global $LETTERS, $LOWERCASE;
  $s = normalize($input_str);
  $freq = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($ch, $freq)) {
  $freq[$ch] = $freq[$ch] + 1;
} else {
  $freq[$ch] = 1;
}
  $i = $i + 1;
};
  $odd = 0;
  foreach (array_keys($freq) as $key) {
  if (fmod($freq[$key], 2) != 0) {
  $odd = $odd + 1;
}
};
  return $odd < 2;
};
  function can_string_be_rearranged_as_palindrome($input_str) {
  global $LETTERS, $LOWERCASE;
  $s = normalize($input_str);
  if (strlen($s) == 0) {
  return true;
}
  $character_freq_dict = [];
  $i = 0;
  while ($i < strlen($s)) {
  $character = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($character, $character_freq_dict)) {
  $character_freq_dict[$character] = $character_freq_dict[$character] + 1;
} else {
  $character_freq_dict[$character] = 1;
}
  $i = $i + 1;
};
  $odd_char = 0;
  foreach (array_keys($character_freq_dict) as $character_key) {
  $character_count = $character_freq_dict[$character_key];
  if ($character_count % 2 != 0) {
  $odd_char = $odd_char + 1;
}
};
  return !($odd_char > 1);
};
  echo rtrim(json_encode(can_string_be_rearranged_as_palindrome_counter('Momo'), 1344)), PHP_EOL;
  echo rtrim(json_encode(can_string_be_rearranged_as_palindrome_counter('Mother'), 1344)), PHP_EOL;
  echo rtrim(json_encode(can_string_be_rearranged_as_palindrome('Momo'), 1344)), PHP_EOL;
  echo rtrim(json_encode(can_string_be_rearranged_as_palindrome('Mother'), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
