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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function generate_large_matrix() {
  global $grid, $results_bin, $results_break, $results_brute, $test_grids;
  $result = [];
  $i = 0;
  while ($i < 1000) {
  $row = [];
  $j = 1000 - $i;
  while ($j > (-1000 - $i)) {
  $row = _append($row, $j);
  $j = $j - 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function find_negative_index($arr) {
  global $grid, $i, $results_bin, $results_break, $results_brute, $test_grids;
  $left = 0;
  $right = count($arr) - 1;
  if (count($arr) == 0) {
  return 0;
}
  if ($arr[0] < 0) {
  return 0;
}
  while ($left <= $right) {
  $mid = _intdiv(($left + $right), 2);
  $num = $arr[$mid];
  if ($num < 0) {
  if ($mid == 0) {
  return 0;
};
  if ($arr[$mid - 1] >= 0) {
  return $mid;
};
  $right = $mid - 1;
} else {
  $left = $mid + 1;
}
};
  return count($arr);
}
function count_negatives_binary_search($grid) {
  global $results_bin, $results_break, $results_brute, $test_grids;
  $total = 0;
  $bound = count($grid[0]);
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $idx = find_negative_index(array_slice($row, 0, $bound));
  $bound = $idx;
  $total = $total + $idx;
  $i = $i + 1;
};
  return (count($grid) * count($grid[0])) - $total;
}
function count_negatives_brute_force($grid) {
  global $results_bin, $results_break, $results_brute, $test_grids;
  $count = 0;
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $j = 0;
  while ($j < count($row)) {
  if ($row[$j] < 0) {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $count;
}
function count_negatives_brute_force_with_break($grid) {
  global $results_bin, $results_break, $results_brute, $test_grids;
  $total = 0;
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $j = 0;
  while ($j < count($row)) {
  $number = $row[$j];
  if ($number < 0) {
  $total = $total + (count($row) - $j);
  break;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $total;
}
$grid = generate_large_matrix();
$test_grids = [[[4, 3, 2, -1], [3, 2, 1, -1], [1, 1, -1, -2], [-1, -1, -2, -3]], [[3, 2], [1, 0]], [[7, 7, 6]], [[7, 7, 6], [-1, -2, -3]], $grid];
$results_bin = [];
$i = 0;
while ($i < count($test_grids)) {
  $results_bin = _append($results_bin, count_negatives_binary_search($test_grids[$i]));
  $i = $i + 1;
}
echo rtrim(_str($results_bin)), PHP_EOL;
$results_brute = [];
$i = 0;
while ($i < count($test_grids)) {
  $results_brute = _append($results_brute, count_negatives_brute_force($test_grids[$i]));
  $i = $i + 1;
}
echo rtrim(_str($results_brute)), PHP_EOL;
$results_break = [];
$i = 0;
while ($i < count($test_grids)) {
  $results_break = _append($results_break, count_negatives_brute_force_with_break($test_grids[$i]));
  $i = $i + 1;
}
echo rtrim(_str($results_break)), PHP_EOL;
