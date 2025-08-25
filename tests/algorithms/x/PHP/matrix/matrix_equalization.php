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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function unique($nums) {
  $res = [];
  $i = 0;
  while ($i < count($nums)) {
  $v = $nums[$i];
  $found = false;
  $j = 0;
  while ($j < count($res)) {
  if ($res[$j] == $v) {
  $found = true;
  break;
}
  $j = $j + 1;
};
  if (!$found) {
  $res = _append($res, $v);
}
  $i = $i + 1;
};
  return $res;
}
function array_equalization($vector, $step_size) {
  if ($step_size <= 0) {
  _panic('Step size must be positive and non-zero.');
}
  $elems = unique($vector);
  $min_updates = count($vector);
  $i = 0;
  while ($i < count($elems)) {
  $target = $elems[$i];
  $idx = 0;
  $updates = 0;
  while ($idx < count($vector)) {
  if ($vector[$idx] != $target) {
  $updates = $updates + 1;
  $idx = $idx + $step_size;
} else {
  $idx = $idx + 1;
}
};
  if ($updates < $min_updates) {
  $min_updates = $updates;
}
  $i = $i + 1;
};
  return $min_updates;
}
echo rtrim(_str(array_equalization([1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2], 4))), PHP_EOL;
echo rtrim(_str(array_equalization([22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92], 2))), PHP_EOL;
echo rtrim(_str(array_equalization([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 5))), PHP_EOL;
echo rtrim(_str(array_equalization([22, 22, 22, 33, 33, 33], 2))), PHP_EOL;
echo rtrim(_str(array_equalization([1, 2, 3], 2147483647))), PHP_EOL;
