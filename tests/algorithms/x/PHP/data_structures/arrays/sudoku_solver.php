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
function string_to_grid($s) {
  global $puzzle;
  $grid = [];
  $i = 0;
  while ($i < 9) {
  $row = [];
  $j = 0;
  while ($j < 9) {
  $ch = substr($s, $i * 9 + $j, $i * 9 + $j + 1 - ($i * 9 + $j));
  $val = 0;
  if ($ch != '0' && $ch != '.') {
  $val = intval($ch);
}
  $row = _append($row, $val);
  $j = $j + 1;
};
  $grid = _append($grid, $row);
  $i = $i + 1;
};
  return $grid;
}
function print_grid($grid) {
  global $puzzle;
  for ($r = 0; $r < 9; $r++) {
  $line = '';
  for ($c = 0; $c < 9; $c++) {
  $line = $line . _str($grid[$r][$c]);
  if ($c < 8) {
  $line = $line . ' ';
}
};
  echo rtrim($line), PHP_EOL;
};
}
function is_safe($grid, $row, $column, $n) {
  global $puzzle;
  for ($i = 0; $i < 9; $i++) {
  if ($grid[$row][$i] == $n || $grid[$i][$column] == $n) {
  return false;
}
};
  for ($i = 0; $i < 3; $i++) {
  for ($j = 0; $j < 3; $j++) {
  if ($grid[($row - $row % 3) + $i][($column - $column % 3) + $j] == $n) {
  return false;
}
};
};
  return true;
}
function find_empty($grid) {
  global $puzzle;
  for ($i = 0; $i < 9; $i++) {
  for ($j = 0; $j < 9; $j++) {
  if ($grid[$i][$j] == 0) {
  return [$i, $j];
}
};
};
  return [];
}
function solve(&$grid) {
  global $puzzle;
  $loc = find_empty($grid);
  if (count($loc) == 0) {
  return true;
}
  $row = $loc[0];
  $column = $loc[1];
  for ($digit = 1; $digit < 10; $digit++) {
  if (is_safe($grid, $row, $column, $digit)) {
  $grid[$row][$column] = $digit;
  if (solve($grid)) {
  return true;
};
  $grid[$row][$column] = 0;
}
};
  return false;
}
$puzzle = '003020600900305001001806400008102900700000008006708200002609500800203009005010300';
$grid = string_to_grid($puzzle);
echo rtrim('Original grid:'), PHP_EOL;
print_grid($grid);
if (solve($grid)) {
  echo rtrim('
Solved grid:'), PHP_EOL;
  print_grid($grid);
} else {
  echo rtrim('
No solution found'), PHP_EOL;
}
