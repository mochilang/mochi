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
function make_list($len, $value) {
  $arr = [];
  $i = 0;
  while ($i < $len) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
}
function min_int($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
}
function min_steps_to_one($number) {
  if ($number <= 0) {
  return 0;
}
  $table = make_list($number + 1, $number + 1);
  $table[1] = 0;
  $i = 1;
  while ($i < $number) {
  $table[$i + 1] = min_int($table[$i + 1], $table[$i] + 1);
  if ($i * 2 <= $number) {
  $table[$i * 2] = min_int($table[$i * 2], $table[$i] + 1);
}
  if ($i * 3 <= $number) {
  $table[$i * 3] = min_int($table[$i * 3], $table[$i] + 1);
}
  $i = $i + 1;
};
  return $table[$number];
}
echo rtrim(_str(min_steps_to_one(10))), PHP_EOL;
echo rtrim(_str(min_steps_to_one(15))), PHP_EOL;
echo rtrim(_str(min_steps_to_one(6))), PHP_EOL;
