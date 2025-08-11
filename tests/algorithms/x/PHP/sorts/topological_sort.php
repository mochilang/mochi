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
$edges = ['a' => ['c', 'b'], 'b' => ['d', 'e'], 'c' => [], 'd' => [], 'e' => []];
$vertices = ['a', 'b', 'c', 'd', 'e'];
function topological_sort($start, &$visited, $sort) {
  global $edges, $vertices;
  $visited[$start] = true;
  $neighbors = $edges[$start];
  $i = 0;
  while ($i < count($neighbors)) {
  $neighbor = $neighbors[$i];
  if (!(array_key_exists($neighbor, $visited))) {
  $sort = topological_sort($neighbor, $visited, $sort);
}
  $i = _iadd($i, 1);
};
  $sort = _append($sort, $start);
  if (count($visited) != count($vertices)) {
  $j = 0;
  while ($j < count($vertices)) {
  $v = $vertices[$j];
  if (!(array_key_exists($v, $visited))) {
  $sort = topological_sort($v, $visited, $sort);
}
  $j = _iadd($j, 1);
};
}
  return $sort;
}
function main() {
  global $edges, $vertices;
  $result = topological_sort('a', [], []);
  echo rtrim(_str($result)), PHP_EOL;
}
main();
