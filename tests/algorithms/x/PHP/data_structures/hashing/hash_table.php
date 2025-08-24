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
$__start_mem = memory_get_usage();
$__start = _now();
  function repeat_int($n, $val) {
  $res = null;
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $val);
  $i = $i + 1;
};
  return $res;
};
  function repeat_bool($n, $val) {
  $res = null;
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $val);
  $i = $i + 1;
};
  return $res;
};
  function set_int($xs, $idx, $value) {
  $res = null;
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function set_bool($xs, $idx, $value) {
  $res = null;
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function create_table($size_table, $charge_factor, $lim_charge) {
  return ['charge_factor' => $charge_factor, 'filled' => repeat_bool($size_table, false), 'lim_charge' => $lim_charge, 'size_table' => $size_table, 'values' => repeat_int($size_table, 0)];
};
  function hash_function($ht, $key) {
  $k = fmod($key, $ht['size_table']);
  if ($k < 0) {
  $k = $k + $ht['size_table'];
}
  return $k;
};
  function is_prime($n) {
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  $i = 3;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 2;
};
  return true;
};
  function next_prime($value, $factor) {
  $candidate = $value * $factor + 1;
  while (!is_prime($candidate)) {
  $candidate = $candidate + 1;
};
  return $candidate;
};
  function set_value($ht, $key, $data) {
  $new_values = set_int($ht['values'], $key, $data);
  $new_filled = set_bool($ht['filled'], $key, true);
  return ['charge_factor' => $ht['charge_factor'], 'filled' => $new_filled, 'lim_charge' => $ht['lim_charge'], 'size_table' => $ht['size_table'], 'values' => $new_values];
};
  function collision_resolution($ht, $key) {
  $new_key = hash_function($ht, $key + 1);
  $steps = 0;
  while ($ht['filled'][$new_key]) {
  $new_key = hash_function($ht, $new_key + 1);
  $steps = $steps + 1;
  if ($steps >= $ht['size_table']) {
  return -1;
}
};
  return $new_key;
};
  function rehashing($ht) {
  $survivors = null;
  $i = 0;
  while ($i < _len($ht['values'])) {
  if ($ht['filled'][$i]) {
  $survivors = _append($survivors, $ht['values'][$i]);
}
  $i = $i + 1;
};
  $new_size = next_prime($ht['size_table'], 2);
  $new_ht = create_table($new_size, $ht['charge_factor'], $ht['lim_charge']);
  $i = 0;
  while ($i < count($survivors)) {
  $new_ht = insert_data($new_ht, $survivors[$i]);
  $i = $i + 1;
};
  return $new_ht;
};
  function insert_data($ht, $data) {
  $key = hash_function($ht, $data);
  if (!$ht['filled'][$key]) {
  return set_value($ht, $key, $data);
}
  if ($ht['values'][$key] == $data) {
  return $ht;
}
  $new_key = collision_resolution($ht, $key);
  if ($new_key >= 0) {
  return set_value($ht, $new_key, $data);
}
  $resized = rehashing($ht);
  return insert_data($resized, $data);
};
  function mochi_keys($ht) {
  $res = null;
  $i = 0;
  while ($i < _len($ht['values'])) {
  if ($ht['filled'][$i]) {
  $res = _append($res, [$i, $ht['values'][$i]]);
}
  $i = $i + 1;
};
  return $res;
};
  function main() {
  $ht = create_table(3, 1, 0.75);
  $ht = insert_data($ht, 17);
  $ht = insert_data($ht, 18);
  $ht = insert_data($ht, 99);
  echo rtrim(json_encode(array_keys($ht), 1344)), PHP_EOL;
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
