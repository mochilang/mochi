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
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function min_slice($a, $start, $end) {
  $m = $a[$start];
  $i = $start + 1;
  while ($i < $end) {
  if ($a[$i] < $m) {
  $m = $a[$i];
}
  $i = $i + 1;
};
  return $m;
}
function max_slice($a, $start, $end) {
  $m = $a[$start];
  $i = $start + 1;
  while ($i < $end) {
  if ($a[$i] > $m) {
  $m = $a[$i];
}
  $i = $i + 1;
};
  return $m;
}
function max_diff_range($a, $start, $end) {
  if ($end - $start == 1) {
  $v = $a[$start];
  return [$v, $v];
}
  $mid = _intdiv(($start + $end), 2);
  $left = max_diff_range($a, $start, $mid);
  $right = max_diff_range($a, $mid, $end);
  $small1 = $left[0];
  $big1 = $left[1];
  $small2 = $right[0];
  $big2 = $right[1];
  $min_left = min_slice($a, $start, $mid);
  $max_right = max_slice($a, $mid, $end);
  $cross_diff = $max_right - $min_left;
  $left_diff = $big1 - $small1;
  $right_diff = $big2 - $small2;
  if ($right_diff > $cross_diff && $right_diff > $left_diff) {
  return [$small2, $big2];
} else {
  if ($left_diff > $cross_diff) {
  return [$small1, $big1];
} else {
  return [$min_left, $max_right];
};
}
}
function max_difference($a) {
  return max_diff_range($a, 0, count($a));
}
function main() {
  $result = max_difference([5, 11, 2, 1, 7, 9, 0, 7]);
  echo rtrim(_str($result)), PHP_EOL;
}
main();
