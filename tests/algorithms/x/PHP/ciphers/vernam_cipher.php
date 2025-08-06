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
  global $plaintext, $key, $encrypted, $decrypted;
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
  global $plaintext, $key, $encrypted, $decrypted;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  return 0;
};
  function mochi_chr($n) {
  global $plaintext, $key, $encrypted, $decrypted;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  return '?';
};
  function vernam_encrypt($plaintext, $key) {
  global $encrypted, $decrypted;
  $ciphertext = '';
  $i = 0;
  while ($i < strlen($plaintext)) {
  $p = mochi_ord(substr($plaintext, $i, $i + 1 - $i)) - 65;
  $k = mochi_ord(substr($key, fmod($i, strlen($key)), fmod($i, strlen($key)) + 1 - fmod($i, strlen($key)))) - 65;
  $ct = $p + $k;
  while ($ct > 25) {
  $ct = $ct - 26;
};
  $ciphertext = $ciphertext . mochi_chr($ct + 65);
  $i = $i + 1;
};
  return $ciphertext;
};
  function vernam_decrypt($ciphertext, $key) {
  global $plaintext, $encrypted;
  $decrypted = '';
  $i = 0;
  while ($i < strlen($ciphertext)) {
  $c = mochi_ord(substr($ciphertext, $i, $i + 1 - $i));
  $k = mochi_ord(substr($key, fmod($i, strlen($key)), fmod($i, strlen($key)) + 1 - fmod($i, strlen($key))));
  $val = $c - $k;
  while ($val < 0) {
  $val = $val + 26;
};
  $decrypted = $decrypted . mochi_chr($val + 65);
  $i = $i + 1;
};
  return $decrypted;
};
  $plaintext = 'HELLO';
  $key = 'KEY';
  $encrypted = vernam_encrypt($plaintext, $key);
  $decrypted = vernam_decrypt($encrypted, $key);
  echo rtrim('Plaintext: ' . $plaintext), PHP_EOL;
  echo rtrim('Encrypted: ' . $encrypted), PHP_EOL;
  echo rtrim('Decrypted: ' . $decrypted), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
