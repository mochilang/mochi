<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function make_table($size_table, $charge_factor) {
  $vals = [];
  $i = 0;
  while ($i < $size_table) {
  $vals = _append($vals, []);
  $i = $i + 1;
};
  return ['size_table' => $size_table, 'charge_factor' => $charge_factor, 'values' => $vals, 'keys' => []];
}
function hash_function($ht, $key) {
  $res = fmod($key, $ht['size_table']);
  if ($res < 0) {
  $res = $res + $ht['size_table'];
}
  return $res;
}
function prepend($lst, $value) {
  $result = [$value];
  $i = 0;
  while ($i < count($lst)) {
  $result = _append($result, $lst[$i]);
  $i = $i + 1;
};
  return $result;
}
function set_value(&$ht, $key, $data) {
  $current = $ht['values'][$key];
  $updated = prepend($current, $data);
  $vals = $ht['values'];
  $vals[$key] = $updated;
  $ht['values'] = $vals;
  $ks = $ht['keys'];
  $ks[$key] = $updated;
  $ht['keys'] = $ks;
}
function count_empty($ht) {
  $count = 0;
  $i = 0;
  while ($i < _len($ht['values'])) {
  if (_len($ht['values'][$i]) == 0) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  return $count;
}
function balanced_factor($ht) {
  $total = 0;
  $i = 0;
  while ($i < _len($ht['values'])) {
  $total = $total + ($ht['charge_factor'] - _len($ht['values'][$i]));
  $i = $i + 1;
};
  return (floatval($total)) / (floatval($ht['size_table'])) * (floatval($ht['charge_factor']));
}
function collision_resolution($ht, $key) {
  if (!(_len($ht['values'][$key]) == $ht['charge_factor'] && count_empty($ht) == 0)) {
  return $key;
}
  $new_key = fmod(($key + 1), $ht['size_table']);
  $steps = 0;
  while (_len($ht['values'][$new_key]) == $ht['charge_factor'] && $steps < $ht['size_table'] - 1) {
  $new_key = fmod(($new_key + 1), $ht['size_table']);
  $steps = $steps + 1;
};
  if (_len($ht['values'][$new_key]) < $ht['charge_factor']) {
  return $new_key;
}
  return -1;
}
function insert(&$ht, $data) {
  $key = hash_function($ht, $data);
  if (_len($ht['values'][$key]) == 0 || _len($ht['values'][$key]) < $ht['charge_factor']) {
  set_value($ht, $key, $data);
  return;
}
  $dest = collision_resolution($ht, $key);
  if ($dest >= 0) {
  set_value($ht, $dest, $data);
} else {
  echo rtrim('table full'), PHP_EOL;
}
}
function main() {
  $ht = make_table(3, 2);
  insert($ht, 10);
  insert($ht, 20);
  insert($ht, 30);
  insert($ht, 40);
  insert($ht, 50);
  echo rtrim(_str($ht['values'])), PHP_EOL;
  echo rtrim(_str(balanced_factor($ht))), PHP_EOL;
}
main();
