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
  function find_char($s, $ch) {
  global $LETTERS;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function encrypt_message($key, $message) {
  global $LETTERS;
  $chars_a = $key;
  $chars_b = $LETTERS;
  $translated = '';
  $i = 0;
  while ($i < strlen($message)) {
  $symbol = substr($message, $i, $i + 1 - $i);
  $upper_sym = strtoupper($symbol);
  $sym_index = find_char($chars_a, $upper_sym);
  if ($sym_index >= 0) {
  $sub_char = substr($chars_b, $sym_index, $sym_index + 1 - $sym_index);
  if ($symbol == $upper_sym) {
  $translated = $translated . strtoupper($sub_char);
} else {
  $translated = $translated . strtolower($sub_char);
};
} else {
  $translated = $translated . $symbol;
}
  $i = $i + 1;
};
  return $translated;
};
  function decrypt_message($key, $message) {
  global $LETTERS;
  $chars_a = $LETTERS;
  $chars_b = $key;
  $translated = '';
  $i = 0;
  while ($i < strlen($message)) {
  $symbol = substr($message, $i, $i + 1 - $i);
  $upper_sym = strtoupper($symbol);
  $sym_index = find_char($chars_a, $upper_sym);
  if ($sym_index >= 0) {
  $sub_char = substr($chars_b, $sym_index, $sym_index + 1 - $sym_index);
  if ($symbol == $upper_sym) {
  $translated = $translated . strtoupper($sub_char);
} else {
  $translated = $translated . strtolower($sub_char);
};
} else {
  $translated = $translated . $symbol;
}
  $i = $i + 1;
};
  return $translated;
};
  function main() {
  global $LETTERS;
  $message = 'Hello World';
  $key = 'QWERTYUIOPASDFGHJKLZXCVBNM';
  $mode = 'decrypt';
  $translated = '';
  if ($mode == 'encrypt') {
  $translated = encrypt_message($key, $message);
} else {
  if ($mode == 'decrypt') {
  $translated = decrypt_message($key, $message);
};
}
  echo rtrim('Using the key ' . $key . ', the ' . $mode . 'ed message is: ' . $translated), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
