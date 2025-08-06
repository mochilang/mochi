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
function min_int($a, $b) {
  global $m1, $m2;
  if ($a < $b) {
  return $a;
}
  return $b;
}
function minimum_cost_path(&$matrix) {
  global $m1, $m2;
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $j = 1;
  while ($j < $cols) {
  $row0 = $matrix[0];
  $row0[$j] = $row0[$j] + $row0[$j - 1];
  $matrix[0] = $row0;
  $j = $j + 1;
};
  $i = 1;
  while ($i < $rows) {
  $row = $matrix[$i];
  $row[0] = $row[0] + $matrix[$i - 1][0];
  $matrix[$i] = $row;
  $i = $i + 1;
};
  $i = 1;
  while ($i < $rows) {
  $row = $matrix[$i];
  $j = 1;
  while ($j < $cols) {
  $up = $matrix[$i - 1][$j];
  $left = $row[$j - 1];
  $best = min_int($up, $left);
  $row[$j] = $row[$j] + $best;
  $j = $j + 1;
};
  $matrix[$i] = $row;
  $i = $i + 1;
};
  return $matrix[$rows - 1][$cols - 1];
}
$m1 = [[2, 1], [3, 1], [4, 2]];
$m2 = [[2, 1, 4], [2, 1, 3], [3, 2, 1]];
echo rtrim(_str(minimum_cost_path($m1))), PHP_EOL;
echo rtrim(_str(minimum_cost_path($m2))), PHP_EOL;
