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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_ord($ch) {
  global $ssc, $encoded;
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return 48 + $i;
}
  $i = $i + 1;
};
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $i = 0;
  while ($i < strlen($upper)) {
  if (substr($upper, $i, $i + 1 - $i) == $ch) {
  return 65 + $i;
}
  $i = $i + 1;
};
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($lower)) {
  if (substr($lower, $i, $i + 1 - $i) == $ch) {
  return 97 + $i;
}
  $i = $i + 1;
};
  return 0;
};
  function neg_pos(&$iterlist) {
  global $ssc, $encoded;
  $i = 1;
  while ($i < count($iterlist)) {
  $iterlist[$i] = -$iterlist[$i];
  $i = $i + 2;
};
  return $iterlist;
};
  function passcode_creator() {
  global $ssc, $encoded;
  $choices = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  $seed = _now();
  $length = 10 + ($seed % 11);
  $password = [];
  $i = 0;
  while ($i < $length) {
  $seed = ($seed * 1103515245 + 12345) % 2147483647;
  $idx = fmod($seed, strlen($choices));
  $password = _append($password, $choices[$idx]);
  $i = $i + 1;
};
  return $password;
};
  function unique_sorted($chars) {
  global $ssc, $encoded;
  $uniq = [];
  $i = 0;
  while ($i < count($chars)) {
  $ch = $chars[$i];
  if (!(in_array($ch, $uniq))) {
  $uniq = _append($uniq, $ch);
}
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($uniq)) {
  $k = $j + 1;
  $min_idx = $j;
  while ($k < count($uniq)) {
  if ($uniq[$k] < $uniq[$min_idx]) {
  $min_idx = $k;
}
  $k = $k + 1;
};
  if ($min_idx != $j) {
  $tmp = $uniq[$j];
  $uniq[$j] = $uniq[$min_idx];
  $uniq[$min_idx] = $tmp;
}
  $j = $j + 1;
};
  return $uniq;
};
  function make_key_list($passcode) {
  global $ssc, $encoded;
  $key_list_options = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~ 	
';
  $breakpoints = unique_sorted($passcode);
  $keys_l = [];
  $temp_list = [];
  $i = 0;
  while ($i < strlen($key_list_options)) {
  $ch = substr($key_list_options, $i, $i + 1 - $i);
  $temp_list = _append($temp_list, $ch);
  if (in_array($ch, $breakpoints) || $i == strlen($key_list_options) - 1) {
  $k = count($temp_list) - 1;
  while ($k >= 0) {
  $keys_l = _append($keys_l, $temp_list[$k]);
  $k = $k - 1;
};
  $temp_list = [];
}
  $i = $i + 1;
};
  return $keys_l;
};
  function make_shift_key($passcode) {
  global $ssc, $encoded;
  $codes = [];
  $i = 0;
  while ($i < count($passcode)) {
  $codes = _append($codes, mochi_ord($passcode[$i]));
  $i = $i + 1;
};
  $codes = neg_pos($codes);
  $total = 0;
  $i = 0;
  while ($i < count($codes)) {
  $total = $total + $codes[$i];
  $i = $i + 1;
};
  if ($total > 0) {
  return $total;
}
  return count($passcode);
};
  function new_cipher($passcode_str) {
  global $ssc, $encoded;
  $passcode = [];
  if (strlen($passcode_str) == 0) {
  $passcode = passcode_creator();
} else {
  $i = 0;
  while ($i < strlen($passcode_str)) {
  $passcode = _append($passcode, $passcode_str[$i]);
  $i = $i + 1;
};
}
  $key_list = make_key_list($passcode);
  $shift_key = make_shift_key($passcode);
  return ['passcode' => $passcode, 'key_list' => $key_list, 'shift_key' => $shift_key];
};
  function index_of($lst, $ch) {
  global $ssc, $encoded;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function encrypt($c, $plaintext) {
  global $ssc;
  $encoded = '';
  $i = 0;
  $n = _len($c['key_list']);
  while ($i < strlen($plaintext)) {
  $ch = substr($plaintext, $i, $i + 1 - $i);
  $position = index_of($c['key_list'], $ch);
  $new_pos = fmod(($position + $c['shift_key']), $n);
  $encoded = $encoded . $c['key_list'][$new_pos];
  $i = $i + 1;
};
  return $encoded;
};
  function decrypt($c, $encoded_message) {
  global $ssc, $encoded;
  $decoded = '';
  $i = 0;
  $n = _len($c['key_list']);
  while ($i < strlen($encoded_message)) {
  $ch = substr($encoded_message, $i, $i + 1 - $i);
  $position = index_of($c['key_list'], $ch);
  $new_pos = fmod(($position - $c['shift_key']), $n);
  if ($new_pos < 0) {
  $new_pos = $new_pos + $n;
}
  $decoded = $decoded . $c['key_list'][$new_pos];
  $i = $i + 1;
};
  return $decoded;
};
  function test_end_to_end() {
  global $ssc, $encoded;
  $msg = 'Hello, this is a modified Caesar cipher';
  $cip = new_cipher('');
  return decrypt($cip, encrypt($cip, $msg));
};
  $ssc = new_cipher('4PYIXyqeQZr44');
  $encoded = encrypt($ssc, 'Hello, this is a modified Caesar cipher');
  echo rtrim($encoded), PHP_EOL;
  echo rtrim(decrypt($ssc, $encoded)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
