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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function int_sqrt($n) {
  $x = 0;
  while (_imul((_iadd($x, 1)), (_iadd($x, 1))) <= $n) {
  $x = _iadd($x, 1);
};
  return $x;
}
function jump_search($arr, $item) {
  $arr_size = count($arr);
  $block_size = int_sqrt($arr_size);
  $prev = 0;
  $step = $block_size;
  while ($step < $arr_size && $arr[_isub($step, 1)] < $item) {
  $prev = $step;
  $step = _iadd($step, $block_size);
  if ($prev >= $arr_size) {
  return -1;
}
};
  while ($prev < $arr_size && $arr[$prev] < $item) {
  $prev = _iadd($prev, 1);
  if ($prev == $step) {
  return -1;
}
};
  if ($prev < $arr_size && $arr[$prev] == $item) {
  return $prev;
}
  return -1;
}
function main() {
  echo rtrim(_str(jump_search([0, 1, 2, 3, 4, 5], 3))), PHP_EOL;
  echo rtrim(_str(jump_search([-5, -2, -1], -1))), PHP_EOL;
  echo rtrim(_str(jump_search([0, 5, 10, 20], 8))), PHP_EOL;
  echo rtrim(_str(jump_search([0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610], 55))), PHP_EOL;
}
main();
