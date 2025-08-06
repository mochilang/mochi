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
$__start_mem = memory_get_usage();
$__start = _now();
  $LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LOWERCASE = 'abcdefghijklmnopqrstuvwxyz';
  $seed = 1;
  function mochi_rand($n) {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  return $seed % $n;
};
  function get_random_key() {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $chars = null;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $chars = _append($chars, $LETTERS[$i]);
  $i = $i + 1;
};
  $j = count($chars) - 1;
  while ($j > 0) {
  $k = mochi_rand($j + 1);
  $tmp = $chars[$j];
  $chars[$j] = $chars[$k];
  $chars[$k] = $tmp;
  $j = $j - 1;
};
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = $i + 1;
};
  return $res;
};
  function check_valid_key($key) {
  global $LETTERS, $LOWERCASE, $seed;
  if (strlen($key) != strlen($LETTERS)) {
  return false;
}
  $used = [];
  $i = 0;
  while ($i < strlen($key)) {
  $ch = substr($key, $i, $i + 1 - $i);
  if ($used[$ch]) {
  return false;
}
  $used[$ch] = true;
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $ch = substr($LETTERS, $i, $i + 1 - $i);
  if (!$used[$ch]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function index_in($s, $ch) {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function char_to_upper($c) {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $i = 0;
  while ($i < strlen($LOWERCASE)) {
  if ($c == substr($LOWERCASE, $i, $i + 1 - $i)) {
  return substr($LETTERS, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $c;
};
  function char_to_lower($c) {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if ($c == substr($LETTERS, $i, $i + 1 - $i)) {
  return substr($LOWERCASE, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $c;
};
  function is_upper($c) {
  global $LETTERS, $LOWERCASE, $seed, $key;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if ($c == substr($LETTERS, $i, $i + 1 - $i)) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function translate_message($key, $message, $mode) {
  global $LETTERS, $LOWERCASE, $seed;
  $chars_a = $LETTERS;
  $chars_b = $key;
  if ($mode == 'decrypt') {
  $tmp = $chars_a;
  $chars_a = $chars_b;
  $chars_b = $tmp;
}
  $translated = '';
  $i = 0;
  while ($i < strlen($message)) {
  $symbol = substr($message, $i, $i + 1 - $i);
  $upper_symbol = char_to_upper($symbol);
  $idx = index_in($chars_a, $upper_symbol);
  if ($idx >= 0) {
  $mapped = substr($chars_b, $idx, $idx + 1 - $idx);
  if (is_upper($symbol)) {
  $translated = $translated . $mapped;
} else {
  $translated = $translated . char_to_lower($mapped);
};
} else {
  $translated = $translated . $symbol;
}
  $i = $i + 1;
};
  return $translated;
};
  function encrypt_message($key, $message) {
  global $LETTERS, $LOWERCASE, $seed;
  $res = translate_message($key, $message, 'encrypt');
  return $res;
};
  function decrypt_message($key, $message) {
  global $LETTERS, $LOWERCASE, $seed;
  $res = translate_message($key, $message, 'decrypt');
  return $res;
};
  $key = 'LFWOAYUISVKMNXPBDCRJTQEGHZ';
  echo rtrim(encrypt_message($key, 'Harshil Darji')), PHP_EOL;
  echo rtrim(decrypt_message($key, 'Ilcrism Olcvs')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
