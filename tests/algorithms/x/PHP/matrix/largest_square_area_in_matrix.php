<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function update_area_of_max_square($row, $col, $rows, $cols, $mat, &$largest_square_area) {
  global $sample;
  if ($row >= $rows || $col >= $cols) {
  return 0;
}
  $right = update_area_of_max_square($row, $col + 1, $rows, $cols, $mat, $largest_square_area);
  $diagonal = update_area_of_max_square($row + 1, $col + 1, $rows, $cols, $mat, $largest_square_area);
  $down = update_area_of_max_square($row + 1, $col, $rows, $cols, $mat, $largest_square_area);
  if ($mat[$row][$col] == 1) {
  $sub = 1 + min([$right, $diagonal, $down]);
  if ($sub > $largest_square_area[0]) {
  $largest_square_area[0] = $sub;
};
  return $sub;
} else {
  return 0;
}
};
  function largest_square_area_in_matrix_top_down($rows, $cols, $mat) {
  global $sample;
  $largest = [0];
  update_area_of_max_square(0, 0, $rows, $cols, $mat, $largest);
  return $largest[0];
};
  function update_area_of_max_square_with_dp($row, $col, $rows, $cols, $mat, &$dp_array, &$largest_square_area) {
  global $sample;
  if ($row >= $rows || $col >= $cols) {
  return 0;
}
  if ($dp_array[$row][$col] != (-1)) {
  return $dp_array[$row][$col];
}
  $right = update_area_of_max_square_with_dp($row, $col + 1, $rows, $cols, $mat, $dp_array, $largest_square_area);
  $diagonal = update_area_of_max_square_with_dp($row + 1, $col + 1, $rows, $cols, $mat, $dp_array, $largest_square_area);
  $down = update_area_of_max_square_with_dp($row + 1, $col, $rows, $cols, $mat, $dp_array, $largest_square_area);
  if ($mat[$row][$col] == 1) {
  $sub = 1 + min([$right, $diagonal, $down]);
  if ($sub > $largest_square_area[0]) {
  $largest_square_area[0] = $sub;
};
  $dp_array[$row][$col] = $sub;
  return $sub;
} else {
  $dp_array[$row][$col] = 0;
  return 0;
}
};
  function largest_square_area_in_matrix_top_down_with_dp($rows, $cols, $mat) {
  global $sample;
  $largest = [0];
  $dp_array = [];
  $r = 0;
  while ($r < $rows) {
  $row_list = [];
  $c = 0;
  while ($c < $cols) {
  $row_list = _append($row_list, -1);
  $c = $c + 1;
};
  $dp_array = _append($dp_array, $row_list);
  $r = $r + 1;
};
  update_area_of_max_square_with_dp(0, 0, $rows, $cols, $mat, $dp_array, $largest);
  return $largest[0];
};
  function largest_square_area_in_matrix_bottom_up($rows, $cols, $mat) {
  global $sample;
  $dp_array = [];
  $r = 0;
  while ($r <= $rows) {
  $row_list = [];
  $c = 0;
  while ($c <= $cols) {
  $row_list = _append($row_list, 0);
  $c = $c + 1;
};
  $dp_array = _append($dp_array, $row_list);
  $r = $r + 1;
};
  $largest = 0;
  $row = $rows - 1;
  while ($row >= 0) {
  $col = $cols - 1;
  while ($col >= 0) {
  $right = $dp_array[$row][$col + 1];
  $diagonal = $dp_array[$row + 1][$col + 1];
  $bottom = $dp_array[$row + 1][$col];
  if ($mat[$row][$col] == 1) {
  $value = 1 + min([$right, $diagonal, $bottom]);
  $dp_array[$row][$col] = $value;
  if ($value > $largest) {
  $largest = $value;
};
} else {
  $dp_array[$row][$col] = 0;
}
  $col = $col - 1;
};
  $row = $row - 1;
};
  return $largest;
};
  function largest_square_area_in_matrix_bottom_up_space_optimization($rows, $cols, $mat) {
  global $sample;
  $current_row = [];
  $i = 0;
  while ($i <= $cols) {
  $current_row = _append($current_row, 0);
  $i = $i + 1;
};
  $next_row = [];
  $j = 0;
  while ($j <= $cols) {
  $next_row = _append($next_row, 0);
  $j = $j + 1;
};
  $largest = 0;
  $row = $rows - 1;
  while ($row >= 0) {
  $col = $cols - 1;
  while ($col >= 0) {
  $right = $current_row[$col + 1];
  $diagonal = $next_row[$col + 1];
  $bottom = $next_row[$col];
  if ($mat[$row][$col] == 1) {
  $value = 1 + min([$right, $diagonal, $bottom]);
  $current_row[$col] = $value;
  if ($value > $largest) {
  $largest = $value;
};
} else {
  $current_row[$col] = 0;
}
  $col = $col - 1;
};
  $next_row = $current_row;
  $current_row = [];
  $t = 0;
  while ($t <= $cols) {
  $current_row = _append($current_row, 0);
  $t = $t + 1;
};
  $row = $row - 1;
};
  return $largest;
};
  $sample = [[1, 1], [1, 1]];
  echo rtrim(json_encode(largest_square_area_in_matrix_top_down(2, 2, $sample), 1344)), PHP_EOL;
  echo rtrim(json_encode(largest_square_area_in_matrix_top_down_with_dp(2, 2, $sample), 1344)), PHP_EOL;
  echo rtrim(json_encode(largest_square_area_in_matrix_bottom_up(2, 2, $sample), 1344)), PHP_EOL;
  echo rtrim(json_encode(largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, $sample), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
