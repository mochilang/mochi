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
  function index_in_string($s, $ch) {
  global $cipher_map, $encoded;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function contains_char($s, $ch) {
  global $cipher_map, $encoded;
  return index_in_string($s, $ch) >= 0;
};
  function is_alpha($ch) {
  global $cipher_map, $encoded;
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  return contains_char($lower, $ch) || contains_char($upper, $ch);
};
  function to_upper($s) {
  global $cipher_map, $encoded;
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $idx = index_in_string($lower, $ch);
  if ($idx >= 0) {
  $res = $res . substr($upper, $idx, $idx + 1 - $idx);
} else {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function remove_duplicates($key) {
  global $cipher_map, $encoded;
  $res = '';
  $i = 0;
  while ($i < strlen($key)) {
  $ch = substr($key, $i, $i + 1 - $i);
  if ($ch == ' ' || (is_alpha($ch) && contains_char($res, $ch) == false)) {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function create_cipher_map($key) {
  global $cipher_map, $encoded;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $cleaned = remove_duplicates(to_upper($key));
  $cipher = [];
  $i = 0;
  while ($i < strlen($cleaned)) {
  $cipher = _append($cipher, $cleaned[$i]);
  $i = $i + 1;
};
  $offset = strlen($cleaned);
  $j = count($cipher);
  while ($j < 26) {
  $char = substr($alphabet, $j - $offset, $j - $offset + 1 - ($j - $offset));
  while (contains_char($cleaned, $char)) {
  $offset = $offset - 1;
  $char = substr($alphabet, $j - $offset, $j - $offset + 1 - ($j - $offset));
};
  $cipher = _append($cipher, $char);
  $j = $j + 1;
};
  return $cipher;
};
  function index_in_list($lst, $ch) {
  global $cipher_map, $encoded;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function encipher($message, $cipher) {
  global $cipher_map, $encoded;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $msg = to_upper($message);
  $res = '';
  $i = 0;
  while ($i < strlen($msg)) {
  $ch = substr($msg, $i, $i + 1 - $i);
  $idx = index_in_string($alphabet, $ch);
  if ($idx >= 0) {
  $res = $res . $cipher[$idx];
} else {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function decipher($message, $cipher) {
  global $cipher_map, $encoded;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $msg = to_upper($message);
  $res = '';
  $i = 0;
  while ($i < strlen($msg)) {
  $ch = substr($msg, $i, $i + 1 - $i);
  $idx = index_in_list($cipher, $ch);
  if ($idx >= 0) {
  $res = $res . substr($alphabet, $idx, $idx + 1 - $idx);
} else {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  $cipher_map = create_cipher_map('Goodbye!!');
  $encoded = encipher('Hello World!!', $cipher_map);
  echo rtrim($encoded), PHP_EOL;
  echo rtrim(decipher($encoded, $cipher_map)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
