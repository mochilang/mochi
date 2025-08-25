<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function mochi_contains($xs, $value) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function sumset($set_a, $set_b) {
  $result = [];
  $i = 0;
  while ($i < count($set_a)) {
  $j = 0;
  while ($j < count($set_b)) {
  $s = $set_a[$i] + $set_b[$j];
  if (!mochi_contains($result, $s)) {
  $result = _append($result, $s);
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $result;
}
function main() {
  $set_a = [1, 2, 3];
  $set_b = [4, 5, 6];
  echo rtrim(_str(sumset($set_a, $set_b))), PHP_EOL;
  $set_c = [4, 5, 6, 7];
  echo rtrim(_str(sumset($set_a, $set_c))), PHP_EOL;
}
main();
