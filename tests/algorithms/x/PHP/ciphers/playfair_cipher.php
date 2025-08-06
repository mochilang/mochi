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
  function contains($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function index_of($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function prepare_input($dirty) {
  $letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $upper_dirty = strtoupper($dirty);
  $filtered = '';
  $i = 0;
  while ($i < strlen($upper_dirty)) {
  $c = substr($upper_dirty, $i, $i + 1 - $i);
  if (strpos($letters, $c) !== false) {
  $filtered = $filtered . $c;
}
  $i = $i + 1;
};
  if (strlen($filtered) < 2) {
  return $filtered;
}
  $clean = '';
  $i = 0;
  while ($i < strlen($filtered) - 1) {
  $c1 = substr($filtered, $i, $i + 1 - $i);
  $c2 = substr($filtered, $i + 1, $i + 2 - ($i + 1));
  $clean = $clean . $c1;
  if ($c1 == $c2) {
  $clean = $clean . 'X';
}
  $i = $i + 1;
};
  $clean = $clean . substr($filtered, strlen($filtered) - 1, strlen($filtered) - (strlen($filtered) - 1));
  if (fmod(strlen($clean), 2) == 1) {
  $clean = $clean . 'X';
}
  return $clean;
};
  function generate_table($key) {
  $alphabet = 'ABCDEFGHIKLMNOPQRSTUVWXYZ';
  $table = [];
  $upper_key = strtoupper($key);
  $i = 0;
  while ($i < strlen($upper_key)) {
  $c = substr($upper_key, $i, $i + 1 - $i);
  if (strpos($alphabet, $c) !== false) {
  if (!(in_array($c, $table))) {
  $table = _append($table, $c);
};
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($alphabet)) {
  $c = substr($alphabet, $i, $i + 1 - $i);
  if (!(in_array($c, $table))) {
  $table = _append($table, $c);
}
  $i = $i + 1;
};
  return $table;
};
  function encode($plaintext, $key) {
  $table = generate_table($key);
  $text = prepare_input($plaintext);
  $cipher = '';
  $i = 0;
  while ($i < strlen($text)) {
  $c1 = substr($text, $i, $i + 1 - $i);
  $c2 = substr($text, $i + 1, $i + 2 - ($i + 1));
  $idx1 = index_of($table, $c1);
  $idx2 = index_of($table, $c2);
  $row1 = _intdiv($idx1, 5);
  $col1 = $idx1 % 5;
  $row2 = _intdiv($idx2, 5);
  $col2 = $idx2 % 5;
  if ($row1 == $row2) {
  $cipher = $cipher . $table[$row1 * 5 + ($col1 + 1) % 5];
  $cipher = $cipher . $table[$row2 * 5 + ($col2 + 1) % 5];
} else {
  if ($col1 == $col2) {
  $cipher = $cipher . $table[(($row1 + 1) % 5) * 5 + $col1];
  $cipher = $cipher . $table[(($row2 + 1) % 5) * 5 + $col2];
} else {
  $cipher = $cipher . $table[$row1 * 5 + $col2];
  $cipher = $cipher . $table[$row2 * 5 + $col1];
};
}
  $i = $i + 2;
};
  return $cipher;
};
  function decode($cipher, $key) {
  $table = generate_table($key);
  $plain = '';
  $i = 0;
  while ($i < strlen($cipher)) {
  $c1 = substr($cipher, $i, $i + 1 - $i);
  $c2 = substr($cipher, $i + 1, $i + 2 - ($i + 1));
  $idx1 = index_of($table, $c1);
  $idx2 = index_of($table, $c2);
  $row1 = _intdiv($idx1, 5);
  $col1 = $idx1 % 5;
  $row2 = _intdiv($idx2, 5);
  $col2 = $idx2 % 5;
  if ($row1 == $row2) {
  $plain = $plain . $table[$row1 * 5 + ($col1 + 4) % 5];
  $plain = $plain . $table[$row2 * 5 + ($col2 + 4) % 5];
} else {
  if ($col1 == $col2) {
  $plain = $plain . $table[(($row1 + 4) % 5) * 5 + $col1];
  $plain = $plain . $table[(($row2 + 4) % 5) * 5 + $col2];
} else {
  $plain = $plain . $table[$row1 * 5 + $col2];
  $plain = $plain . $table[$row2 * 5 + $col1];
};
}
  $i = $i + 2;
};
  return $plain;
};
  function main() {
  echo rtrim('Encoded:') . " " . rtrim(encode('BYE AND THANKS', 'GREETING')), PHP_EOL;
  echo rtrim('Decoded:') . " " . rtrim(decode('CXRBANRLBALQ', 'GREETING')), PHP_EOL;
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
