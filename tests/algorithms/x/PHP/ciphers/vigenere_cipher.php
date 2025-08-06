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
$__start_mem = memory_get_usage();
$__start = _now();
  $LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LETTERS_LOWER = 'abcdefghijklmnopqrstuvwxyz';
  function find_index($s, $ch) {
  global $LETTERS, $LETTERS_LOWER, $key, $message, $key_up, $encrypted, $key_index, $symbol, $upper_symbol, $num, $decrypted;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function to_upper_char($ch) {
  global $LETTERS, $LETTERS_LOWER, $key, $message, $key_up, $encrypted, $key_index, $i, $symbol, $upper_symbol, $num, $decrypted;
  $idx = find_index($LETTERS_LOWER, $ch);
  if ($idx >= 0) {
  return substr($LETTERS, $idx, $idx + 1 - $idx);
}
  return $ch;
};
  function to_lower_char($ch) {
  global $LETTERS, $LETTERS_LOWER, $key, $message, $key_up, $encrypted, $key_index, $i, $symbol, $upper_symbol, $num, $decrypted;
  $idx = find_index($LETTERS, $ch);
  if ($idx >= 0) {
  return substr($LETTERS_LOWER, $idx, $idx + 1 - $idx);
}
  return $ch;
};
  function is_upper($ch) {
  global $LETTERS, $LETTERS_LOWER, $key, $message, $key_up, $encrypted, $key_index, $i, $symbol, $upper_symbol, $num, $decrypted;
  return find_index($LETTERS, $ch) >= 0;
};
  function to_upper_string($s) {
  global $LETTERS, $LETTERS_LOWER, $key, $message, $key_up, $encrypted, $key_index, $symbol, $upper_symbol, $num, $decrypted;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $res = $res . to_upper_char(substr($s, $i, $i + 1 - $i));
  $i = $i + 1;
};
  return $res;
};
  $key = 'HDarji';
  $message = 'This is Harshil Darji from Dharmaj.';
  $key_up = to_upper_string($key);
  $encrypted = '';
  $key_index = 0;
  $i = 0;
  while ($i < strlen($message)) {
  $symbol = substr($message, $i, $i + 1 - $i);
  $upper_symbol = to_upper_char($symbol);
  $num = find_index($LETTERS, $upper_symbol);
  if ($num >= 0) {
  $num = $num + find_index($LETTERS, substr($key_up, $key_index, $key_index + 1 - $key_index));
  $num = fmod($num, strlen($LETTERS));
  if (is_upper($symbol)) {
  $encrypted = $encrypted . substr($LETTERS, $num, $num + 1 - $num);
} else {
  $encrypted = $encrypted . to_lower_char(substr($LETTERS, $num, $num + 1 - $num));
};
  $key_index = $key_index + 1;
  if ($key_index == strlen($key_up)) {
  $key_index = 0;
};
} else {
  $encrypted = $encrypted . $symbol;
}
  $i = $i + 1;
}
  echo rtrim($encrypted), PHP_EOL;
  $decrypted = '';
  $key_index = 0;
  $i = 0;
  while ($i < strlen($encrypted)) {
  $symbol = substr($encrypted, $i, $i + 1 - $i);
  $upper_symbol = to_upper_char($symbol);
  $num = find_index($LETTERS, $upper_symbol);
  if ($num >= 0) {
  $num = $num - find_index($LETTERS, substr($key_up, $key_index, $key_index + 1 - $key_index));
  $num = fmod($num, strlen($LETTERS));
  if (is_upper($symbol)) {
  $decrypted = $decrypted . substr($LETTERS, $num, $num + 1 - $num);
} else {
  $decrypted = $decrypted . to_lower_char(substr($LETTERS, $num, $num + 1 - $num));
};
  $key_index = $key_index + 1;
  if ($key_index == strlen($key_up)) {
  $key_index = 0;
};
} else {
  $decrypted = $decrypted . $symbol;
}
  $i = $i + 1;
}
  echo rtrim($decrypted), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
