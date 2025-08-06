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
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  function to_upper($s) {
  global $UPPER, $LOWER;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $found = false;
  while ($j < 26) {
  if ($ch == substr($LOWER, $j, $j + 1 - $j)) {
  $res = $res . substr($UPPER, $j, $j + 1 - $j);
  $found = true;
  break;
}
  $j = $j + 1;
};
  if ($found == false) {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function contains($xs, $x) {
  global $UPPER, $LOWER;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function contains_char($s, $ch) {
  global $UPPER, $LOWER;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function get_value($keys, $values, $key) {
  global $UPPER, $LOWER;
  $i = 0;
  while ($i < count($keys)) {
  if ($keys[$i] == $key) {
  return $values[$i];
}
  $i = $i + 1;
};
  return null;
};
  function print_mapping($keys, $values) {
  global $UPPER, $LOWER;
  $s = '{';
  $i = 0;
  while ($i < count($keys)) {
  $s = $s . '\'' . $keys[$i] . '\': \'' . $values[$i] . '\'';
  if ($i + 1 < count($keys)) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . '}';
  echo rtrim($s), PHP_EOL;
};
  function mixed_keyword($keyword, $plaintext, $verbose) {
  global $UPPER, $LOWER;
  $alphabet = $UPPER;
  $keyword_u = to_upper($keyword);
  $plaintext_u = to_upper($plaintext);
  $unique = [];
  $i = 0;
  while ($i < strlen($keyword_u)) {
  $ch = substr($keyword_u, $i, $i + 1 - $i);
  if (contains_char($alphabet, $ch) && in_array($ch, $unique) == false) {
  $unique = _append($unique, $ch);
}
  $i = $i + 1;
};
  $num_unique = count($unique);
  $shifted = [];
  $i = 0;
  while ($i < count($unique)) {
  $shifted = _append($shifted, $unique[$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($alphabet)) {
  $ch = substr($alphabet, $i, $i + 1 - $i);
  if (in_array($ch, $unique) == false) {
  $shifted = _append($shifted, $ch);
}
  $i = $i + 1;
};
  $modified = [];
  $k = 0;
  while ($k < count($shifted)) {
  $row = [];
  $r = 0;
  while ($r < $num_unique && $k + $r < count($shifted)) {
  $row = _append($row, $shifted[$k + $r]);
  $r = $r + 1;
};
  $modified = _append($modified, $row);
  $k = $k + $num_unique;
};
  $keys = [];
  $values = [];
  $column = 0;
  $letter_index = 0;
  while ($column < $num_unique) {
  $row_idx = 0;
  while ($row_idx < count($modified)) {
  $row = $modified[$row_idx];
  if (count($row) <= $column) {
  break;
}
  $keys = _append($keys, $alphabet[$letter_index]);
  $values = _append($values, $row[$column]);
  $letter_index = $letter_index + 1;
  $row_idx = $row_idx + 1;
};
  $column = $column + 1;
};
  if ($verbose) {
  print_mapping($keys, $values);
}
  $result = '';
  $i = 0;
  while ($i < strlen($plaintext_u)) {
  $ch = substr($plaintext_u, $i, $i + 1 - $i);
  $mapped = get_value($keys, $values, $ch);
  if ($mapped == null) {
  $result = $result . $ch;
} else {
  $result = $result . $mapped;
}
  $i = $i + 1;
};
  return $result;
};
  echo rtrim(mixed_keyword('college', 'UNIVERSITY', true)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
