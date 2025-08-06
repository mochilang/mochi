<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function repeat_int($n, $val) {
  $res = null;
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $val);
  $i = $i + 1;
};
  return $res;
}
function repeat_bool($n, $val) {
  $res = null;
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $val);
  $i = $i + 1;
};
  return $res;
}
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
}
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
}
function create_table($size_table, $charge_factor, $lim_charge) {
  return ['size_table' => $size_table, 'values' => repeat_int($size_table, 0), 'filled' => repeat_bool($size_table, false), 'charge_factor' => $charge_factor, 'lim_charge' => $lim_charge];
}
function hash_function($ht, $key) {
  $k = fmod($key, $ht['size_table']);
  if ($k < 0) {
  $k = $k + $ht['size_table'];
}
  return $k;
}
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
}
function next_prime($value, $factor) {
  $candidate = $value * $factor + 1;
  while (!is_prime($candidate)) {
  $candidate = $candidate + 1;
};
  return $candidate;
}
function set_value($ht, $key, $data) {
  $new_values = set_int($ht['values'], $key, $data);
  $new_filled = set_bool($ht['filled'], $key, true);
  return ['size_table' => $ht['size_table'], 'values' => $new_values, 'filled' => $new_filled, 'charge_factor' => $ht['charge_factor'], 'lim_charge' => $ht['lim_charge']];
}
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
}
function rehashing($ht) {
  $survivors = null;
  $i = 0;
  while ($i < _len($ht['values'])) {
  if ($ht['filled'][$i]) {
  $survivors = _append($survivors, $ht[$values][$i]);
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
}
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
}
function keys($ht) {
  $res = null;
  $i = 0;
  while ($i < _len($ht['values'])) {
  if ($ht['filled'][$i]) {
  $res = _append($res, [$i, $ht[$values][$i]]);
}
  $i = $i + 1;
};
  return $res;
}
function main() {
  $ht = create_table(3, 1, 0.75);
  $ht = insert_data($ht, 17);
  $ht = insert_data($ht, 18);
  $ht = insert_data($ht, 99);
  echo rtrim(json_encode(array_keys($ht), 1344)), PHP_EOL;
}
main();
