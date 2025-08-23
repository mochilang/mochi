<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function string_to_chars($s) {
  $chars = [];
  $i = 0;
  while ($i < strlen($s)) {
  $chars = _append($chars, $s[$i]);
  $i = $i + 1;
};
  return $chars;
};
  function join_chars($chars) {
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = $i + 1;
};
  return $res;
};
  function insert_at($chars, $index, $ch) {
  $res = [];
  $i = 0;
  while ($i < $index) {
  $res = _append($res, $chars[$i]);
  $i = $i + 1;
};
  $res = _append($res, $ch);
  while ($i < count($chars)) {
  $res = _append($res, $chars[$i]);
  $i = $i + 1;
};
  return $res;
};
  function remove_at($chars, $index) {
  $res = [];
  $i = 0;
  while ($i < count($chars)) {
  if ($i != $index) {
  $res = _append($res, $chars[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function make_matrix_int($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
};
  function make_matrix_string($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
};
  function compute_transform_tables($source_string, $destination_string, $copy_cost, $replace_cost, $delete_cost, $insert_cost) {
  $source_seq = string_to_chars($source_string);
  $dest_seq = string_to_chars($destination_string);
  $m = count($source_seq);
  $n = count($dest_seq);
  $costs = make_matrix_int($m + 1, $n + 1, 0);
  $ops = make_matrix_string($m + 1, $n + 1, '0');
  $i = 1;
  while ($i <= $m) {
  $costs[$i][0] = $i * $delete_cost;
  $ops[$i][0] = 'D' . $source_seq[$i - 1];
  $i = $i + 1;
};
  $j = 1;
  while ($j <= $n) {
  $costs[0][$j] = $j * $insert_cost;
  $ops[0][$j] = 'I' . $dest_seq[$j - 1];
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $m) {
  $j = 1;
  while ($j <= $n) {
  if ($source_seq[$i - 1] == $dest_seq[$j - 1]) {
  $costs[$i][$j] = $costs[$i - 1][$j - 1] + $copy_cost;
  $ops[$i][$j] = 'C' . $source_seq[$i - 1];
} else {
  $costs[$i][$j] = $costs[$i - 1][$j - 1] + $replace_cost;
  $ops[$i][$j] = 'R' . $source_seq[$i - 1] . $dest_seq[$j - 1];
}
  if ($costs[$i - 1][$j] + $delete_cost < $costs[$i][$j]) {
  $costs[$i][$j] = $costs[$i - 1][$j] + $delete_cost;
  $ops[$i][$j] = 'D' . $source_seq[$i - 1];
}
  if ($costs[$i][$j - 1] + $insert_cost < $costs[$i][$j]) {
  $costs[$i][$j] = $costs[$i][$j - 1] + $insert_cost;
  $ops[$i][$j] = 'I' . $dest_seq[$j - 1];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ['costs' => $costs, 'ops' => $ops];
};
  function assemble_transformation($ops, $i, $j) {
  if ($i == 0 && $j == 0) {
  return [];
}
  $op = $ops[$i][$j];
  $kind = substr($op, 0, 1);
  if ($kind == 'C' || $kind == 'R') {
  $seq = assemble_transformation($ops, $i - 1, $j - 1);
  $seq = _append($seq, $op);
  return $seq;
} else {
  if ($kind == 'D') {
  $seq = assemble_transformation($ops, $i - 1, $j);
  $seq = _append($seq, $op);
  return $seq;
} else {
  $seq = assemble_transformation($ops, $i, $j - 1);
  $seq = _append($seq, $op);
  return $seq;
};
}
};
  function main() {
  $copy_cost = -1;
  $replace_cost = 1;
  $delete_cost = 2;
  $insert_cost = 2;
  $src = 'Python';
  $dst = 'Algorithms';
  $tables = compute_transform_tables($src, $dst, $copy_cost, $replace_cost, $delete_cost, $insert_cost);
  $operations = $tables['ops'];
  $m = count($operations);
  $n = count($operations[0]);
  $sequence = assemble_transformation($operations, $m - 1, $n - 1);
  $string_list = string_to_chars($src);
  $idx = 0;
  $cost = 0;
  $k = 0;
  while ($k < count($sequence)) {
  echo rtrim(join_chars($string_list)), PHP_EOL;
  $op = $sequence[$k];
  $kind = substr($op, 0, 1);
  if ($kind == 'C') {
  $cost = $cost + $copy_cost;
} else {
  if ($kind == 'R') {
  $string_list[$idx] = substr($op, 2, 3 - 2);
  $cost = $cost + $replace_cost;
} else {
  if ($kind == 'D') {
  $string_list = remove_at($string_list, $idx);
  $cost = $cost + $delete_cost;
} else {
  $string_list = insert_at($string_list, $idx, substr($op, 1, 2 - 1));
  $cost = $cost + $insert_cost;
};
};
}
  $idx = $idx + 1;
  $k = $k + 1;
};
  echo rtrim(join_chars($string_list)), PHP_EOL;
  echo rtrim('Cost: ' . _str($cost)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
