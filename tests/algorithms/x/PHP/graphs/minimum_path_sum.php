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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fill_row($current_row, $row_above) {
  global $grid1, $grid2;
  $current_row[0] = $current_row[0] + $row_above[0];
  $cell_n = 1;
  while ($cell_n < count($current_row)) {
  $left = $current_row[$cell_n - 1];
  $up = $row_above[$cell_n];
  if ($left < $up) {
  $current_row[$cell_n] = $current_row[$cell_n] + $left;
} else {
  $current_row[$cell_n] = $current_row[$cell_n] + $up;
}
  $cell_n = $cell_n + 1;
};
  return $current_row;
};
  function min_path_sum(&$grid) {
  global $grid1, $grid2;
  if (count($grid) == 0 || count($grid[0]) == 0) {
  _panic('The grid does not contain the appropriate information');
}
  $cell_n = 1;
  while ($cell_n < count($grid[0])) {
  $grid[0][$cell_n] = $grid[0][$cell_n] + $grid[0][$cell_n - 1];
  $cell_n = $cell_n + 1;
};
  $row_above = $grid[0];
  $row_n = 1;
  while ($row_n < count($grid)) {
  $current_row = $grid[$row_n];
  $grid[$row_n] = fill_row($current_row, $row_above);
  $row_above = $grid[$row_n];
  $row_n = $row_n + 1;
};
  return $grid[count($grid) - 1][count($grid[0]) - 1];
};
  $grid1 = [[1, 3, 1], [1, 5, 1], [4, 2, 1]];
  echo rtrim(_str(min_path_sum($grid1))), PHP_EOL;
  $grid2 = [[1, 0, 5, 6, 7], [8, 9, 0, 4, 2], [4, 4, 4, 5, 1], [9, 6, 3, 1, 0], [8, 4, 3, 2, 7]];
  echo rtrim(_str(min_path_sum($grid2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
