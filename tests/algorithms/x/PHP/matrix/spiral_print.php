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
function is_valid_matrix($matrix) {
  if (count($matrix) == 0) {
  return false;
}
  $cols = count($matrix[0]);
  foreach ($matrix as $row) {
  if (count($row) != $cols) {
  return false;
}
};
  return true;
}
function spiral_traversal($matrix) {
  if (!is_valid_matrix($matrix)) {
  return [];
}
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $top = 0;
  $bottom = $rows - 1;
  $left = 0;
  $right = $cols - 1;
  $result = [];
  while ($left <= $right && $top <= $bottom) {
  $i = $left;
  while ($i <= $right) {
  $result = _append($result, $matrix[$top][$i]);
  $i = $i + 1;
};
  $top = $top + 1;
  $i = $top;
  while ($i <= $bottom) {
  $result = _append($result, $matrix[$i][$right]);
  $i = $i + 1;
};
  $right = $right - 1;
  if ($top <= $bottom) {
  $i = $right;
  while ($i >= $left) {
  $result = _append($result, $matrix[$bottom][$i]);
  $i = $i - 1;
};
  $bottom = $bottom - 1;
}
  if ($left <= $right) {
  $i = $bottom;
  while ($i >= $top) {
  $result = _append($result, $matrix[$i][$left]);
  $i = $i - 1;
};
  $left = $left + 1;
}
};
  return $result;
}
function spiral_print_clockwise($matrix) {
  foreach (spiral_traversal($matrix) as $value) {
  echo rtrim(_str($value)), PHP_EOL;
};
}
function main() {
  $a = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]];
  spiral_print_clockwise($a);
  echo rtrim(_str(spiral_traversal($a))), PHP_EOL;
}
main();
