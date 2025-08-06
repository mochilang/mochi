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
function is_safe($grid, $row, $column, $n) {
  global $initial_grid, $no_solution, $examples, $idx;
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
function find_empty_location($grid) {
  global $initial_grid, $no_solution, $examples, $idx;
  for ($i = 0; $i < 9; $i++) {
  for ($j = 0; $j < 9; $j++) {
  if ($grid[$i][$j] == 0) {
  return [$i, $j];
}
};
};
  return [];
}
function sudoku(&$grid) {
  global $initial_grid, $no_solution, $examples, $idx;
  $loc = find_empty_location($grid);
  if (count($loc) == 0) {
  return true;
}
  $row = $loc[0];
  $column = $loc[1];
  for ($digit = 1; $digit < 10; $digit++) {
  if (is_safe($grid, $row, $column, $digit)) {
  $grid[$row][$column] = $digit;
  if (sudoku($grid)) {
  return true;
};
  $grid[$row][$column] = 0;
}
};
  return false;
}
function print_solution($grid) {
  global $initial_grid, $no_solution, $examples, $idx;
  for ($r = 0; $r < count($grid); $r++) {
  $line = '';
  for ($c = 0; $c < count($grid[$r]); $c++) {
  $line = $line . _str($grid[$r][$c]);
  if ($c < count($grid[$r]) - 1) {
  $line = $line . ' ';
}
};
  echo rtrim($line), PHP_EOL;
};
}
$initial_grid = [[3, 0, 6, 5, 0, 8, 4, 0, 0], [5, 2, 0, 0, 0, 0, 0, 0, 0], [0, 8, 7, 0, 0, 0, 0, 3, 1], [0, 0, 3, 0, 1, 0, 0, 8, 0], [9, 0, 0, 8, 6, 3, 0, 0, 5], [0, 5, 0, 0, 9, 0, 6, 0, 0], [1, 3, 0, 0, 0, 0, 2, 5, 0], [0, 0, 0, 0, 0, 0, 0, 7, 4], [0, 0, 5, 2, 0, 6, 3, 0, 0]];
$no_solution = [[5, 0, 6, 5, 0, 8, 4, 0, 3], [5, 2, 0, 0, 0, 0, 0, 0, 2], [1, 8, 7, 0, 0, 0, 0, 3, 1], [0, 0, 3, 0, 1, 0, 0, 8, 0], [9, 0, 0, 8, 6, 3, 0, 0, 5], [0, 5, 0, 0, 9, 0, 6, 0, 0], [1, 3, 0, 0, 0, 0, 2, 5, 0], [0, 0, 0, 0, 0, 0, 0, 7, 4], [0, 0, 5, 2, 0, 6, 3, 0, 0]];
$examples = [$initial_grid, $no_solution];
$idx = 0;
while ($idx < count($examples)) {
  echo rtrim('
Example grid:
===================='), PHP_EOL;
  print_solution($examples[$idx]);
  echo rtrim('
Example grid solution:'), PHP_EOL;
  if (sudoku($examples[$idx])) {
  print_solution($examples[$idx]);
} else {
  echo rtrim('Cannot find a solution.'), PHP_EOL;
}
  $idx = $idx + 1;
}
