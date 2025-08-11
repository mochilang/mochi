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
function max_tasks($tasks_info) {
  $order = [];
  $i = 0;
  while ($i < count($tasks_info)) {
  $order = _append($order, $i);
  $i = _iadd($i, 1);
};
  $n = count($order);
  $i = 0;
  while ($i < $n) {
  $j = _iadd($i, 1);
  while ($j < $n) {
  if ($tasks_info[$order[$j]][1] > $tasks_info[$order[$i]][1]) {
  $tmp = $order[$i];
  $order[$i] = $order[$j];
  $order[$j] = $tmp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  $result = [];
  $pos = 1;
  $i = 0;
  while ($i < $n) {
  $id = $order[$i];
  $deadline = $tasks_info[$id][0];
  if ($deadline >= $pos) {
  $result = _append($result, $id);
}
  $i = _iadd($i, 1);
  $pos = _iadd($pos, 1);
};
  return $result;
}
function main() {
  $ex1 = [[4, 20], [1, 10], [1, 40], [1, 30]];
  $ex2 = [[1, 10], [2, 20], [3, 30], [2, 40]];
  echo rtrim(_str(max_tasks($ex1))), PHP_EOL;
  echo rtrim(_str(max_tasks($ex2))), PHP_EOL;
}
main();
