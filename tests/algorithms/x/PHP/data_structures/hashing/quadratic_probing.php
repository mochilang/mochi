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
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function create_hash_table($size) {
  global $qp;
  $vals = [];
  $i = 0;
  while ($i < $size) {
  $vals = _append($vals, null);
  $i = $i + 1;
};
  return ['lim_charge' => 0.75, 'size_table' => $size, 'values' => $vals];
};
  function hash_function($table, $key) {
  global $qp;
  return fmod($key, $table['size_table']);
};
  function balanced_factor($table) {
  global $qp;
  $count = 0;
  $i = 0;
  while ($i < _len($table['values'])) {
  if ($table['values'][$i] != null) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  return (floatval($count)) / (floatval($table['size_table']));
};
  function collision_resolution($table, $key) {
  global $qp;
  $i = 1;
  $new_key = hash_function($table, $key + $i * $i);
  while ($table['values'][$new_key] != null && $table['values'][$new_key] != $key) {
  $i = $i + 1;
  if (balanced_factor($table) >= $table['lim_charge']) {
  return $table['size_table'];
}
  $new_key = hash_function($table, $key + $i * $i);
};
  return $new_key;
};
  function insert_data(&$table, $data) {
  global $qp;
  $key = hash_function($table, $data);
  $vals = $table['values'];
  if ($vals[$key] == null) {
  $vals[$key] = $data;
} else {
  if ($vals[$key] == $data) {
  $table['values'] = $vals;
  return;
} else {
  $new_key = collision_resolution($table, $key);
  if ($new_key < count($vals) && $vals[$new_key] == null) {
  $vals[$new_key] = $data;
};
};
}
  $table['values'] = $vals;
};
  function int_to_string($n) {
  global $qp;
  if ($n == 0) {
  return '0';
}
  $num = $n;
  $neg = false;
  if ($num < 0) {
  $neg = true;
  $num = -$num;
}
  $res = '';
  while ($num > 0) {
  $digit = $num % 10;
  $ch = substr('0123456789', $digit, $digit + 1 - $digit);
  $res = $ch . $res;
  $num = _intdiv($num, 10);
};
  if ($neg) {
  $res = '-' . $res;
}
  return $res;
};
  function keys_to_string($table) {
  global $qp;
  $result = '{';
  $first = true;
  $i = 0;
  while ($i < _len($table['values'])) {
  $v = $table['values'][$i];
  if ($v != null) {
  if (!$first) {
  $result = $result . ', ';
};
  $result = $result . int_to_string($i) . ': ' . int_to_string($v);
  $first = false;
}
  $i = $i + 1;
};
  $result = $result . '}';
  return $result;
};
  $qp = create_hash_table(8);
  insert_data($qp, 0);
  insert_data($qp, 999);
  insert_data($qp, 111);
  echo rtrim(keys_to_string($qp)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
