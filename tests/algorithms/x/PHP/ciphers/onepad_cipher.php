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
  $seed = 1;
  function set_seed($s) {
  global $seed, $ascii_chars, $res, $cipher, $key;
  $seed = $s;
};
  function randint($a, $b) {
  global $seed, $ascii_chars, $res, $cipher, $key;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return ($seed % ($b - $a + 1)) + $a;
};
  $ascii_chars = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  function mochi_ord($ch) {
  global $seed, $ascii_chars, $res, $cipher, $key;
  $i = 0;
  while ($i < strlen($ascii_chars)) {
  if (substr($ascii_chars, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
};
  function mochi_chr($code) {
  global $seed, $ascii_chars, $res, $cipher, $key;
  if ($code < 32 || $code > 126) {
  return '';
}
  return substr($ascii_chars, $code - 32, $code - 32 + 1 - ($code - 32));
};
  function encrypt($text) {
  global $seed, $ascii_chars;
  $cipher = [];
  $key = [];
  $i = 0;
  while ($i < strlen($text)) {
  $p = mochi_ord(substr($text, $i, $i + 1 - $i));
  $k = randint(1, 300);
  $c = ($p + $k) * $k;
  $cipher = _append($cipher, $c);
  $key = _append($key, $k);
  $i = $i + 1;
};
  $res = [];
  $res['cipher'] = $cipher;
  $res['key'] = $key;
  return $res;
};
  function decrypt($cipher, $key) {
  global $seed, $ascii_chars, $res;
  $plain = '';
  $i = 0;
  while ($i < count($key)) {
  $p = (($cipher[$i] - $key[$i] * $key[$i]) / $key[$i]);
  $plain = $plain . mochi_chr($p);
  $i = $i + 1;
};
  return $plain;
};
  set_seed(1);
  $res = encrypt('Hello');
  $cipher = $res['cipher'];
  $key = $res['key'];
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($cipher, 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($key, 1344))))))), PHP_EOL;
  echo rtrim(decrypt($cipher, $key)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
