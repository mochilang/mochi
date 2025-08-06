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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function join_strings($xs) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function encrypt_message($key, $message) {
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
  $num_cols = (strlen($message) + $key - 1) / $key;
  $num_rows = $key;
  $num_shaded_boxes = ($num_cols * $num_rows) - strlen($message);
  $plain_text = [];
  $i = 0;
  while ($i < $num_cols) {
  $plain_text = _append($plain_text, '');
  $i = $i + 1;
};
  $col = 0;
  $row = 0;
  $index = 0;
  while ($index < strlen($message)) {
  $plain_text[$col] = $plain_text[$col] . substr($message, $index, $index + 1 - $index);
  $col = $col + 1;
  if ($col == $num_cols || ($col == $num_cols - 1 && $row >= $num_rows - $num_shaded_boxes)) {
  $col = 0;
  $row = $row + 1;
}
  $index = $index + 1;
};
  return join_strings($plain_text);
};
  function main() {
  echo rtrim('Enter message: '), PHP_EOL;
  $message = trim(fgets(STDIN));
  $max_key = strlen($message) - 1;
  echo rtrim('Enter key [2-' . _str($max_key) . ']: '), PHP_EOL;
  $key = intval(trim(fgets(STDIN)));
  echo rtrim('Encryption/Decryption [e/d]: '), PHP_EOL;
  $mode = trim(fgets(STDIN));
  $text = '';
  $first = substr($mode, 0, 1 - 0);
  if ($first == 'e' || $first == 'E') {
  $text = encrypt_message($key, $message);
} else {
  if ($first == 'd' || $first == 'D') {
  $text = decrypt_message($key, $message);
};
}
  echo rtrim('Output:
' . $text . '|'), PHP_EOL;
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
