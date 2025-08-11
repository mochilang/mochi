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
function min3($a, $b, $c) {
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
}
function edit_distance($source, $target) {
  if (strlen($source) == 0) {
  return strlen($target);
}
  if (strlen($target) == 0) {
  return strlen($source);
}
  $last_source = substr($source, _isub(strlen($source), 1), strlen($source) - _isub(strlen($source), 1));
  $last_target = substr($target, _isub(strlen($target), 1), strlen($target) - _isub(strlen($target), 1));
  $delta = ($last_source == $last_target ? 0 : 1);
  $delete_cost = _iadd(edit_distance(substr($source, 0, _isub(strlen($source), 1)), $target), 1);
  $insert_cost = _iadd(edit_distance($source, substr($target, 0, _isub(strlen($target), 1))), 1);
  $replace_cost = _iadd(edit_distance(substr($source, 0, _isub(strlen($source), 1)), substr($target, 0, _isub(strlen($target), 1))), $delta);
  return min3($delete_cost, $insert_cost, $replace_cost);
}
function main() {
  $result = edit_distance('ATCGCTG', 'TAGCTAA');
  echo rtrim(_str($result)), PHP_EOL;
}
main();
