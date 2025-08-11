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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function heapify(&$arr, $index, $heap_size) {
  global $data, $result;
  $largest = $index;
  $left_index = _iadd(_imul(2, $index), 1);
  $right_index = _iadd(_imul(2, $index), 2);
  if ($left_index < $heap_size && $arr[$left_index] > $arr[$largest]) {
  $largest = $left_index;
}
  if ($right_index < $heap_size && $arr[$right_index] > $arr[$largest]) {
  $largest = $right_index;
}
  if ($largest != $index) {
  $temp = $arr[$largest];
  $arr[$largest] = $arr[$index];
  $arr[$index] = $temp;
  heapify($arr, $largest, $heap_size);
}
}
function heap_sort($arr) {
  global $data, $result;
  $n = count($arr);
  $i = _isub(_intdiv($n, 2), 1);
  while ($i >= 0) {
  heapify($arr, $i, $n);
  $i = _isub($i, 1);
};
  $i = _isub($n, 1);
  while ($i > 0) {
  $temp = $arr[0];
  $arr[0] = $arr[$i];
  $arr[$i] = $temp;
  heapify($arr, 0, $i);
  $i = _isub($i, 1);
};
  return $arr;
}
$data = [3, 7, 9, 28, 123, -5, 8, -30, -200, 0, 4];
$result = heap_sort($data);
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($result, 1344)))))), PHP_EOL;
if (_str($result) != _str([-200, -30, -5, 0, 3, 4, 7, 8, 9, 28, 123])) {
  _panic('Assertion error');
}
