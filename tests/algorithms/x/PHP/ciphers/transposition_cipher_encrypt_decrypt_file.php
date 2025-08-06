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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function encrypt_message($key, $message) {
  global $encrypted, $decrypted;
  $result = '';
  $col = 0;
  while ($col < $key) {
  $pointer = $col;
  while ($pointer < strlen($message)) {
  $result = $result . substr($message, $pointer, $pointer + 1 - $pointer);
  $pointer = $pointer + $key;
};
  $col = $col + 1;
};
  return $result;
};
  function decrypt_message($key, $message) {
  global $encrypted, $decrypted;
  $msg_len = strlen($message);
  $num_cols = _intdiv($msg_len, $key);
  if ($msg_len % $key != 0) {
  $num_cols = $num_cols + 1;
}
  $num_rows = $key;
  $num_shaded_boxes = $num_cols * $num_rows - $msg_len;
  $plain = [];
  $i = 0;
  while ($i < $num_cols) {
  $plain = _append($plain, '');
  $i = $i + 1;
};
  $col = 0;
  $row = 0;
  $idx = 0;
  while ($idx < $msg_len) {
  $ch = substr($message, $idx, $idx + 1 - $idx);
  $plain[$col] = $plain[$col] . $ch;
  $col = $col + 1;
  if ($col == $num_cols || ($col == $num_cols - 1 && $row >= $num_rows - $num_shaded_boxes)) {
  $col = 0;
  $row = $row + 1;
}
  $idx = $idx + 1;
};
  $result = '';
  $i = 0;
  while ($i < $num_cols) {
  $result = $result . $plain[$i];
  $i = $i + 1;
};
  return $result;
};
  $key = 6;
  $message = 'Harshil Darji';
  $encrypted = encrypt_message($key, $message);
  echo rtrim($encrypted), PHP_EOL;
  $decrypted = decrypt_message($key, $encrypted);
  echo rtrim($decrypted), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
