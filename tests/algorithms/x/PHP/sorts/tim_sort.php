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
function copy_list($xs) {
  global $sample, $sorted_sample, $sample2, $sorted_sample2;
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  $res = _append($res, $xs[$k]);
  $k = _iadd($k, 1);
};
  return $res;
}
function insertion_sort($xs) {
  global $sample, $sorted_sample, $sample2, $sorted_sample2;
  $arr = copy_list($xs);
  $idx = 1;
  while ($idx < count($arr)) {
  $value = $arr[$idx];
  $jdx = _isub($idx, 1);
  while ($jdx >= 0 && $arr[$jdx] > $value) {
  $arr[_iadd($jdx, 1)] = $arr[$jdx];
  $jdx = _isub($jdx, 1);
};
  $arr[_iadd($jdx, 1)] = $value;
  $idx = _iadd($idx, 1);
};
  return $arr;
}
function merge($left, $right) {
  global $sample, $sorted_sample, $sample2, $sorted_sample2;
  $result = [];
  $i = 0;
  $j = 0;
  while ($i < count($left) && $j < count($right)) {
  if ($left[$i] < $right[$j]) {
  $result = _append($result, $left[$i]);
  $i = _iadd($i, 1);
} else {
  $result = _append($result, $right[$j]);
  $j = _iadd($j, 1);
}
};
  while ($i < count($left)) {
  $result = _append($result, $left[$i]);
  $i = _iadd($i, 1);
};
  while ($j < count($right)) {
  $result = _append($result, $right[$j]);
  $j = _iadd($j, 1);
};
  return $result;
}
function tim_sort($xs) {
  global $sample, $sorted_sample, $sample2, $sorted_sample2;
  $n = count($xs);
  $runs = [];
  $sorted_runs = [];
  $current = [];
  $current = _append($current, $xs[0]);
  $i = 1;
  while ($i < $n) {
  if ($xs[$i] < $xs[_isub($i, 1)]) {
  $runs = _append($runs, copy_list($current));
  $current = [];
  $current = _append($current, $xs[$i]);
} else {
  $current = _append($current, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  $runs = _append($runs, copy_list($current));
  $r = 0;
  while ($r < count($runs)) {
  $sorted_runs = _append($sorted_runs, insertion_sort($runs[$r]));
  $r = _iadd($r, 1);
};
  $result = [];
  $r = 0;
  while ($r < count($sorted_runs)) {
  $result = merge($result, $sorted_runs[$r]);
  $r = _iadd($r, 1);
};
  return $result;
}
function list_to_string($xs) {
  global $sample, $sorted_sample, $sample2, $sorted_sample2;
  $s = '[';
  $k = 0;
  while ($k < count($xs)) {
  $s = $s . _str($xs[$k]);
  if ($k < _isub(count($xs), 1)) {
  $s = $s . ', ';
}
  $k = _iadd($k, 1);
};
  return $s . ']';
}
$sample = [5, 9, 10, 3, -4, 5, 178, 92, 46, -18, 0, 7];
$sorted_sample = tim_sort($sample);
echo rtrim(list_to_string($sorted_sample)), PHP_EOL;
$sample2 = [3, 2, 1];
$sorted_sample2 = tim_sort($sample2);
echo rtrim(list_to_string($sorted_sample2)), PHP_EOL;
