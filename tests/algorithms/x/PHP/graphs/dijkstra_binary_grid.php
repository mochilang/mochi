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
function mochi_key($p) {
  global $grid1, $grid2;
  return _str($p['x']) . ',' . _str($p['y']);
}
function path_to_string($path) {
  global $grid1, $grid2;
  $s = '[';
  $i = 0;
  while ($i < count($path)) {
  $pt = $path[$i];
  $s = $s . '(' . _str($pt['x']) . ', ' . _str($pt['y']) . ')';
  if ($i < count($path) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function dijkstra($grid, $source, $destination, $allow_diagonal) {
  global $grid1, $grid2;
  $rows = count($grid);
  $cols = count($grid[0]);
  $dx = [-1, 1, 0, 0];
  $dy = [0, 0, -1, 1];
  if ($allow_diagonal) {
  $dx = array_merge($dx, [-1, -1, 1, 1]);
  $dy = array_merge($dy, [-1, 1, -1, 1]);
}
  $INF = 1000000000000.0;
  $queue = [$source];
  $front = 0;
  $dist_map = [mochi_key($source) => 0.0];
  $prev = [];
  while ($front < count($queue)) {
  $current = $queue[$front];
  $front = $front + 1;
  $cur_key = mochi_key($current);
  if ($current['x'] == $destination['x'] && $current['y'] == $destination['y']) {
  break;
}
  $i = 0;
  while ($i < count($dx)) {
  $nx = $current['x'] + $dx[$i];
  $ny = $current['y'] + $dy[$i];
  if ($nx >= 0 && $nx < $rows && $ny >= 0 && $ny < $cols) {
  if ($grid[$nx][$ny] == 1) {
  $n_key = _str($nx) . ',' . _str($ny);
  if (!(array_key_exists($n_key, $dist_map))) {
  $dist_map[$n_key] = $dist_map[$cur_key] + 1.0;
  $prev[$n_key] = $current;
  $queue = _append($queue, ['x' => $nx, 'y' => $ny]);
};
};
}
  $i = $i + 1;
};
};
  $dest_key = mochi_key($destination);
  if (array_key_exists($dest_key, $dist_map)) {
  $path_rev = [$destination];
  $step_key = $dest_key;
  $step_pt = $destination;
  while ($step_key != mochi_key($source)) {
  $step_pt = $prev[$step_key];
  $step_key = mochi_key($step_pt);
  $path_rev = _append($path_rev, $step_pt);
};
  $path = [];
  $k = count($path_rev) - 1;
  while ($k >= 0) {
  $path = _append($path, $path_rev[$k]);
  $k = $k - 1;
};
  return ['distance' => $dist_map[$dest_key], 'path' => $path];
}
  return ['distance' => $INF, 'path' => []];
}
function print_result($res) {
  global $grid1, $grid2;
  echo rtrim(_str($res['distance']) . ', ' . path_to_string($res['path'])), PHP_EOL;
}
$grid1 = [[1, 1, 1], [0, 1, 0], [0, 1, 1]];
print_result(dijkstra($grid1, ['x' => 0, 'y' => 0], ['x' => 2, 'y' => 2], false));
print_result(dijkstra($grid1, ['x' => 0, 'y' => 0], ['x' => 2, 'y' => 2], true));
$grid2 = [[1, 1, 1], [0, 0, 1], [0, 1, 1]];
print_result(dijkstra($grid2, ['x' => 0, 'y' => 0], ['x' => 2, 'y' => 2], false));
