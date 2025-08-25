<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function is_safe($grid, $visited, $row, $col) {
  $rows = count($grid);
  $cols = count($grid[0]);
  $within_bounds = $row >= 0 && $row < $rows && $col >= 0 && $col < $cols;
  if (!$within_bounds) {
  return false;
}
  $visited_cell = $visited[$row][$col];
  $not_visited = $visited_cell == false;
  return $not_visited && $grid[$row][$col] == 1;
}
function dfs($grid, &$visited, $row, $col) {
  $row_nbr = [-1, -1, -1, 0, 0, 1, 1, 1];
  $col_nbr = [-1, 0, 1, -1, 1, -1, 0, 1];
  $visited[$row][$col] = true;
  $k = 0;
  while ($k < 8) {
  $new_row = $row + $row_nbr[$k];
  $new_col = $col + $col_nbr[$k];
  if (is_safe($grid, $visited, $new_row, $new_col)) {
  dfs($grid, $visited, $new_row, $new_col);
}
  $k = $k + 1;
};
}
function count_islands($grid) {
  $rows = count($grid);
  $cols = count($grid[0]);
  $visited = [];
  $i = 0;
  while ($i < $rows) {
  $row_list = [];
  $j = 0;
  while ($j < $cols) {
  $row_list = _append($row_list, false);
  $j = $j + 1;
};
  $visited = _append($visited, $row_list);
  $i = $i + 1;
};
  $count = 0;
  $i = 0;
  while ($i < $rows) {
  $j = 0;
  while ($j < $cols) {
  if (!$visited[$i][$j] && $grid[$i][$j] == 1) {
  dfs($grid, $visited, $i, $j);
  $count = $count + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $count;
}
$grid = [[1, 1, 0, 0, 0], [0, 1, 0, 0, 1], [1, 0, 0, 1, 1], [0, 0, 0, 0, 0], [1, 0, 1, 0, 1]];
echo rtrim(json_encode(count_islands($grid), 1344)), PHP_EOL;
