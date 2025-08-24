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
  function is_prime($n) {
  if ($n < 2) {
  return false;
}
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function prev_prime($n) {
  $p = $n - 1;
  while ($p >= 2) {
  if (is_prime($p)) {
  return $p;
}
  $p = $p - 1;
};
  return 1;
};
  function create_table($size) {
  $vals = [];
  $i = 0;
  while ($i < $size) {
  $vals = _append($vals, (-1));
  $i = $i + 1;
};
  return $vals;
};
  function hash1($size, $key) {
  return $key % $size;
};
  function hash2($prime, $key) {
  return $prime - ($key % $prime);
};
  function insert_double_hash($values, $size, $prime, $value) {
  $vals = $values;
  $idx = hash1($size, $value);
  $step = hash2($prime, $value);
  $count = 0;
  while ($vals[$idx] != (-1) && $count < $size) {
  $idx = ($idx + $step) % $size;
  $count = $count + 1;
};
  if ($vals[$idx] == (-1)) {
  $vals[$idx] = $value;
}
  return $vals;
};
  function table_keys($values) {
  $res = [];
  $i = 0;
  while ($i < count($values)) {
  if ($values[$i] != (-1)) {
  $res[$i] = $values[$i];
}
  $i = $i + 1;
};
  return $res;
};
  function run_example($size, $data) {
  $prime = prev_prime($size);
  $table = create_table($size);
  $i = 0;
  while ($i < count($data)) {
  $table = insert_double_hash($table, $size, $prime, $data[$i]);
  $i = $i + 1;
};
  echo rtrim(_str(table_keys($table))), PHP_EOL;
};
  run_example(3, [10, 20, 30]);
  run_example(4, [10, 20, 30]);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
