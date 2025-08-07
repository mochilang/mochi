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
  function get_neighbours($p, $x_limit, $y_limit) {
  global $world_x, $world_y, $start, $goal, $path, $world;
  $deltas = [['x' => (0 - 1), 'y' => (0 - 1)], ['x' => (0 - 1), 'y' => 0], ['x' => (0 - 1), 'y' => 1], ['x' => 0, 'y' => (0 - 1)], ['x' => 0, 'y' => 1], ['x' => 1, 'y' => (0 - 1)], ['x' => 1, 'y' => 0], ['x' => 1, 'y' => 1]];
  $neighbours = [];
  foreach ($deltas as $d) {
  $nx = $p['x'] + $d['x'];
  $ny = $p['y'] + $d['y'];
  if (0 <= $nx && $nx < $x_limit && 0 <= $ny && $ny < $y_limit) {
  $neighbours = _append($neighbours, ['x' => $nx, 'y' => $ny]);
}
};
  return $neighbours;
};
  function contains($nodes, $p) {
  global $world_x, $world_y, $start, $goal, $path, $world;
  foreach ($nodes as $n) {
  if ($n['pos']['x'] == $p['x'] && $n['pos']['y'] == $p['y']) {
  return true;
}
};
  return false;
};
  function get_node($nodes, $p) {
  global $world_x, $world_y, $start, $goal, $path, $world;
  foreach ($nodes as $n) {
  if ($n['pos']['x'] == $p['x'] && $n['pos']['y'] == $p['y']) {
  return $n;
}
};
  return ['pos' => $p, 'parent' => ['x' => (0 - 1), 'y' => (0 - 1)], 'g' => 0, 'h' => 0, 'f' => 0];
};
  function astar($x_limit, $y_limit, $start, $goal) {
  global $world_x, $world_y, $world;
  $open = [];
  $closed = [];
  $open = _append($open, ['pos' => $start, 'parent' => ['x' => (0 - 1), 'y' => (0 - 1)], 'g' => 0, 'h' => 0, 'f' => 0]);
  $current = $open[0];
  while (count($open) > 0) {
  $min_index = 0;
  $i = 1;
  while ($i < count($open)) {
  if ($open[$i]['f'] < $open[$min_index]['f']) {
  $min_index = $i;
}
  $i = $i + 1;
};
  $current = $open[$min_index];
  $new_open = [];
  $j = 0;
  while ($j < count($open)) {
  if ($j != $min_index) {
  $new_open = _append($new_open, $open[$j]);
}
  $j = $j + 1;
};
  $open = $new_open;
  $closed = _append($closed, $current);
  if ($current['pos']['x'] == $goal['x'] && $current['pos']['y'] == $goal['y']) {
  break;
}
  $neighbours = get_neighbours($current['pos'], $x_limit, $y_limit);
  foreach ($neighbours as $np) {
  if (contains($closed, $np)) {
  continue;
}
  $g = $current['g'] + 1;
  $dx = $goal['x'] - $np['x'];
  $dy = $goal['y'] - $np['y'];
  $h = $dx * $dx + $dy * $dy;
  $f = $g + $h;
  $skip = false;
  foreach ($open as $node) {
  if ($node['pos']['x'] == $np['x'] && $node['pos']['y'] == $np['y'] && $node['f'] < $f) {
  $skip = true;
}
};
  if ($skip) {
  continue;
}
  $open = _append($open, ['pos' => $np, 'parent' => $current['pos'], $g => $g, $h => $h, $f => $f]);
};
};
  $path = [];
  $path = _append($path, $current['pos']);
  while (!($current['parent']['x'] == (0 - 1) && $current['parent']['y'] == (0 - 1))) {
  $current = get_node($closed, $current['parent']);
  $path = _append($path, $current['pos']);
};
  $rev = [];
  $k = count($path) - 1;
  while ($k >= 0) {
  $rev = _append($rev, $path[$k]);
  $k = $k - 1;
};
  return $rev;
};
  function create_world($x_limit, $y_limit) {
  global $world_x, $world_y, $start, $goal, $path;
  $world = [];
  $i = 0;
  while ($i < $x_limit) {
  $row = [];
  $j = 0;
  while ($j < $y_limit) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $world = _append($world, $row);
  $i = $i + 1;
};
  return $world;
};
  function mark_path(&$world, $path) {
  global $world_x, $world_y, $start, $goal;
  foreach ($path as $p) {
  $world[$p['x']][$p['y']] = 1;
};
};
  function print_world($world) {
  global $world_x, $world_y, $start, $goal, $path;
  foreach ($world as $row) {
  echo rtrim(_str($row)), PHP_EOL;
};
};
  $world_x = 5;
  $world_y = 5;
  $start = ['x' => 0, 'y' => 0];
  $goal = ['x' => 4, 'y' => 4];
  $path = astar($world_x, $world_y, $start, $goal);
  echo rtrim('path from (' . _str($start['x']) . ', ' . _str($start['y']) . ') to (' . _str($goal['x']) . ', ' . _str($goal['y']) . ')'), PHP_EOL;
  $world = create_world($world_x, $world_y);
  mark_path($world, $path);
  print_world($world);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
