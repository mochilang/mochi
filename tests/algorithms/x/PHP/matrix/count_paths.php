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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function depth_first_search($grid, $row, $col, &$visit) {
  $row_length = count($grid);
  $col_length = count($grid[0]);
  if ($row < 0 || $col < 0 || $row == $row_length || $col == $col_length) {
  return 0;
}
  if ($visit[$row][$col]) {
  return 0;
}
  if ($grid[$row][$col] == 1) {
  return 0;
}
  if ($row == $row_length - 1 && $col == $col_length - 1) {
  return 1;
}
  $visit[$row][$col] = true;
  $count = 0;
  $count = $count + depth_first_search($grid, $row + 1, $col, $visit);
  $count = $count + depth_first_search($grid, $row - 1, $col, $visit);
  $count = $count + depth_first_search($grid, $row, $col + 1, $visit);
  $count = $count + depth_first_search($grid, $row, $col - 1, $visit);
  $visit[$row][$col] = false;
  return $count;
};
  function count_paths($grid) {
  $rows = count($grid);
  $cols = count($grid[0]);
  $visit = [];
  $i = 0;
  while ($i < $rows) {
  $row_visit = [];
  $j = 0;
  while ($j < $cols) {
  $row_visit = _append($row_visit, false);
  $j = $j + 1;
};
  $visit = _append($visit, $row_visit);
  $i = $i + 1;
};
  return depth_first_search($grid, 0, 0, $visit);
};
  function main() {
  $grid1 = [[0, 0, 0, 0], [1, 1, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]];
  echo rtrim(_str(count_paths($grid1))), PHP_EOL;
  $grid2 = [[0, 0, 0, 0, 0], [0, 1, 1, 1, 0], [0, 1, 1, 1, 0], [0, 0, 0, 0, 0]];
  echo rtrim(_str(count_paths($grid2))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
