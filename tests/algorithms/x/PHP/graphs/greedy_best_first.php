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
  function mochi_abs($x) {
  global $TEST_GRIDS, $delta;
  if ($x < 0) {
  return 0 - $x;
}
  return $x;
};
  function manhattan($x1, $y1, $x2, $y2) {
  global $TEST_GRIDS, $delta;
  return mochi_abs($x1 - $x2) + mochi_abs($y1 - $y2);
};
  function clone_path($p) {
  global $TEST_GRIDS, $delta;
  $res = [];
  $i = 0;
  while ($i < count($p)) {
  $res = _append($res, $p[$i]);
  $i = $i + 1;
};
  return $res;
};
  function make_node($pos_x, $pos_y, $goal_x, $goal_y, $g_cost, $path) {
  global $TEST_GRIDS, $delta;
  $f = manhattan($pos_x, $pos_y, $goal_x, $goal_y);
  return ['pos_x' => $pos_x, 'pos_y' => $pos_y, 'goal_x' => $goal_x, 'goal_y' => $goal_y, 'g_cost' => $g_cost, 'f_cost' => $f, 'path' => $path];
};
  $delta = [['y' => -1, 'x' => 0], ['y' => 0, 'x' => -1], ['y' => 1, 'x' => 0], ['y' => 0, 'x' => 1]];
  function node_equal($a, $b) {
  global $TEST_GRIDS, $delta;
  return $a['pos_x'] == $b['pos_x'] && $a['pos_y'] == $b['pos_y'];
};
  function mochi_contains($nodes, $node) {
  global $TEST_GRIDS, $delta;
  $i = 0;
  while ($i < count($nodes)) {
  if (node_equal($nodes[$i], $node)) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function sort_nodes($nodes) {
  global $TEST_GRIDS, $delta;
  $arr = $nodes;
  $i = 1;
  while ($i < count($arr)) {
  $key_node = $arr[$i];
  $j = $i - 1;
  while ($j >= 0) {
  $temp = $arr[$j];
  if ($temp['f_cost'] > $key_node['f_cost']) {
  $arr[$j + 1] = $temp;
  $j = $j - 1;
} else {
  break;
}
};
  $arr[$j + 1] = $key_node;
  $i = $i + 1;
};
  return $arr;
};
  function get_successors($grid, $parent, $target) {
  global $TEST_GRIDS, $delta;
  $res = [];
  $i = 0;
  while ($i < count($delta)) {
  $d = $delta[$i];
  $pos_x = $parent['pos_x'] + $d['x'];
  $pos_y = $parent['pos_y'] + $d['y'];
  if ($pos_x >= 0 && $pos_x < count($grid[0]) && $pos_y >= 0 && $pos_y < count($grid) && $grid[$pos_y][$pos_x] == 0) {
  $new_path = clone_path($parent['path']);
  $new_path = _append($new_path, ['y' => $pos_y, 'x' => $pos_x]);
  $res = _append($res, make_node($pos_x, $pos_y, $target['x'], $target['y'], $parent['g_cost'] + 1, $new_path));
}
  $i = $i + 1;
};
  return $res;
};
  function greedy_best_first($grid, $init, $goal) {
  global $TEST_GRIDS, $delta;
  $start_path = [$init];
  $start = make_node($init['x'], $init['y'], $goal['x'], $goal['y'], 0, $start_path);
  $open_nodes = [$start];
  $closed_nodes = [];
  while (count($open_nodes) > 0) {
  $open_nodes = sort_nodes($open_nodes);
  $current = $open_nodes[0];
  $new_open = [];
  $idx = 1;
  while ($idx < count($open_nodes)) {
  $new_open = _append($new_open, $open_nodes[$idx]);
  $idx = $idx + 1;
};
  $open_nodes = $new_open;
  if ($current['pos_x'] == $goal['x'] && $current['pos_y'] == $goal['y']) {
  return $current['path'];
}
  $closed_nodes = _append($closed_nodes, $current);
  $successors = get_successors($grid, $current, $goal);
  $i = 0;
  while ($i < count($successors)) {
  $child = $successors[$i];
  if ((!mochi_contains($closed_nodes, $child)) && (!mochi_contains($open_nodes, $child))) {
  $open_nodes = _append($open_nodes, $child);
}
  $i = $i + 1;
};
};
  $r = [$init];
  return $r;
};
  $TEST_GRIDS = [[[0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0], [1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 0, 0]], [[0, 0, 0, 1, 1, 0, 0], [0, 0, 0, 0, 1, 0, 1], [0, 0, 0, 1, 1, 0, 0], [0, 1, 0, 0, 1, 0, 0], [1, 0, 0, 1, 1, 0, 1], [0, 0, 0, 0, 0, 0, 0]], [[0, 0, 1, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 1], [1, 0, 0, 1, 1], [0, 0, 0, 0, 0]]];
  function print_grid($grid) {
  global $TEST_GRIDS, $delta;
  $i = 0;
  while ($i < count($grid)) {
  echo rtrim(_str($grid[$i])), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  global $TEST_GRIDS, $delta;
  $idx = 0;
  while ($idx < count($TEST_GRIDS)) {
  echo rtrim('==grid-' . _str($idx + 1) . '=='), PHP_EOL;
  $grid = $TEST_GRIDS[$idx];
  $init = ['y' => 0, 'x' => 0];
  $goal = ['y' => count($grid) - 1, 'x' => count($grid[0]) - 1];
  print_grid($grid);
  echo rtrim('------'), PHP_EOL;
  $path = greedy_best_first($grid, $init, $goal);
  $j = 0;
  while ($j < count($path)) {
  $p = $path[$j];
  $grid[$p['y']][$p['x']] = 2;
  $j = $j + 1;
};
  print_grid($grid);
  $idx = $idx + 1;
};
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
