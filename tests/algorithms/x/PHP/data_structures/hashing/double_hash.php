<?php
ini_set('memory_limit', '-1');
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
}
function prev_prime($n) {
  $p = $n - 1;
  while ($p >= 2) {
  if (is_prime($p)) {
  return $p;
}
  $p = $p - 1;
};
  return 1;
}
function create_table($size) {
  $vals = [];
  $i = 0;
  while ($i < $size) {
  $vals = _append($vals, (-1));
  $i = $i + 1;
};
  return $vals;
}
function hash1($size, $key) {
  return $key % $size;
}
function hash2($prime, $key) {
  return $prime - ($key % $prime);
}
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
}
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
}
function run_example($size, $data) {
  $prime = prev_prime($size);
  $table = create_table($size);
  $i = 0;
  while ($i < count($data)) {
  $table = insert_double_hash($table, $size, $prime, $data[$i]);
  $i = $i + 1;
};
  echo rtrim(_str(table_keys($table))), PHP_EOL;
}
run_example(3, [10, 20, 30]);
run_example(4, [10, 20, 30]);
