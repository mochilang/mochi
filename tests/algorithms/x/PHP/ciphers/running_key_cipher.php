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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  global $key, $plaintext, $ciphertext;
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
  global $key, $plaintext, $ciphertext;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = _indexof($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  return 0;
};
  function mochi_chr($n) {
  global $key, $plaintext, $ciphertext;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  return '?';
};
  function clean_text($s) {
  global $key, $plaintext, $ciphertext;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= 'A' && $ch <= 'Z') {
  $out = $out . $ch;
} else {
  if ($ch >= 'a' && $ch <= 'z') {
  $out = $out . mochi_chr(mochi_ord($ch) - 32);
};
}
  $i = $i + 1;
};
  return $out;
};
  function running_key_encrypt($key, $plaintext) {
  global $ciphertext;
  $pt = clean_text($plaintext);
  $k = clean_text($key);
  $key_len = strlen($k);
  $res = '';
  $ord_a = mochi_ord('A');
  $i = 0;
  while ($i < strlen($pt)) {
  $p = mochi_ord(substr($pt, $i, $i + 1 - $i)) - $ord_a;
  $kv = mochi_ord(substr($k, $i % $key_len, $i % $key_len + 1 - $i % $key_len)) - $ord_a;
  $c = ($p + $kv) % 26;
  $res = $res . mochi_chr($c + $ord_a);
  $i = $i + 1;
};
  return $res;
};
  function running_key_decrypt($key, $ciphertext) {
  global $plaintext;
  $ct = clean_text($ciphertext);
  $k = clean_text($key);
  $key_len = strlen($k);
  $res = '';
  $ord_a = mochi_ord('A');
  $i = 0;
  while ($i < strlen($ct)) {
  $c = mochi_ord(substr($ct, $i, $i + 1 - $i)) - $ord_a;
  $kv = mochi_ord(substr($k, $i % $key_len, $i % $key_len + 1 - $i % $key_len)) - $ord_a;
  $p = ($c - $kv + 26) % 26;
  $res = $res . mochi_chr($p + $ord_a);
  $i = $i + 1;
};
  return $res;
};
  $key = 'How does the duck know that? said Victor';
  $plaintext = 'DEFEND THIS';
  $ciphertext = running_key_encrypt($key, $plaintext);
  echo rtrim($ciphertext), PHP_EOL;
  echo rtrim(running_key_decrypt($key, $ciphertext)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
