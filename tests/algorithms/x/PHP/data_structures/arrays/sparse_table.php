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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function pow2($n) {
  global $st1, $st2;
  $result = 1;
  $i = 0;
  while ($i < $n) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function int_log2($n) {
  global $st1, $st2;
  $v = $n;
  $res = 0;
  while ($v > 1) {
  $v = _intdiv($v, 2);
  $res = $res + 1;
};
  return $res;
}
function build_sparse_table($number_list) {
  global $st1, $st2;
  if (count($number_list) == 0) {
  $panic('empty number list not allowed');
}
  $length = count($number_list);
  $row = int_log2($length) + 1;
  $sparse_table = [];
  $j = 0;
  while ($j < $row) {
  $inner = [];
  $i = 0;
  while ($i < $length) {
  $inner = _append($inner, 0);
  $i = $i + 1;
};
  $sparse_table = _append($sparse_table, $inner);
  $j = $j + 1;
};
  $i = 0;
  while ($i < $length) {
  $sparse_table[0][$i] = $number_list[$i];
  $i = $i + 1;
};
  $j = 1;
  while (pow2($j) <= $length) {
  $i = 0;
  while ($i + pow2($j) - 1 < $length) {
  $left = $sparse_table[$j - 1][$i + pow2($j - 1)];
  $right = $sparse_table[$j - 1][$i];
  if ($left < $right) {
  $sparse_table[$j][$i] = $left;
} else {
  $sparse_table[$j][$i] = $right;
}
  $i = $i + 1;
};
  $j = $j + 1;
};
  return $sparse_table;
}
function query($sparse_table, $left_bound, $right_bound) {
  global $st1, $st2;
  if ($left_bound < 0 || $right_bound >= count($sparse_table[0])) {
  $panic('list index out of range');
}
  $interval = $right_bound - $left_bound + 1;
  $j = int_log2($interval);
  $val1 = $sparse_table[$j][$right_bound - pow2($j) + 1];
  $val2 = $sparse_table[$j][$left_bound];
  if ($val1 < $val2) {
  return $val1;
}
  return $val2;
}
$st1 = build_sparse_table([8, 1, 0, 3, 4, 9, 3]);
echo rtrim(_str($st1)), PHP_EOL;
$st2 = build_sparse_table([3, 1, 9]);
echo rtrim(_str($st2)), PHP_EOL;
echo rtrim(_str(query($st1, 0, 4))), PHP_EOL;
echo rtrim(_str(query($st1, 4, 6))), PHP_EOL;
echo rtrim(_str(query($st2, 2, 2))), PHP_EOL;
echo rtrim(_str(query($st2, 0, 1))), PHP_EOL;
