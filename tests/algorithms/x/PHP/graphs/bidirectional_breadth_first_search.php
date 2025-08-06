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
$grid = [[0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0], [1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 0, 0]];
$delta = [[-1, 0], [0, -1], [1, 0], [0, 1]];
function mochi_key($y, $x) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  return _str($y) . ',' . _str($x);
}
function parse_int($s) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
}
function parse_key($k) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  $idx = 0;
  while ($idx < strlen($k) && substr($k, $idx, $idx + 1 - $idx) != ',') {
  $idx = $idx + 1;
};
  $y = parse_int(substr($k, 0, $idx - 0));
  $x = parse_int(substr($k, $idx + 1, strlen($k) - ($idx + 1)));
  return [$y, $x];
}
function neighbors($pos) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  $coords = parse_key($pos);
  $y = $coords[0];
  $x = $coords[1];
  $res = [];
  $i = 0;
  while ($i < count($delta)) {
  $ny = $y + $delta[$i][0];
  $nx = $x + $delta[$i][1];
  if ($ny >= 0 && $ny < count($grid) && $nx >= 0 && $nx < count($grid[0])) {
  if ($grid[$ny][$nx] == 0) {
  $res = _append($res, mochi_key($ny, $nx));
};
}
  $i = $i + 1;
};
  return $res;
}
function reverse_list($lst) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = count($lst) - 1;
  while ($i >= 0) {
  $res = _append($res, $lst[$i]);
  $i = $i - 1;
};
  return $res;
}
function bfs($start, $goal) {
  global $grid, $delta, $path1, $path2;
  $queue = [];
  $queue = _append($queue, ['pos' => $start, 'path' => [$start]]);
  $head = 0;
  $visited = [$start => true];
  while ($head < count($queue)) {
  $node = $queue[$head];
  $head = $head + 1;
  if ($node['pos'] == $goal) {
  return $node['path'];
}
  $neigh = neighbors($node['pos']);
  $i = 0;
  while ($i < count($neigh)) {
  $npos = $neigh[$i];
  if (!(array_key_exists($npos, $visited))) {
  $visited[$npos] = true;
  $new_path = _append($node['path'], $npos);
  $queue = _append($queue, ['pos' => $npos, 'path' => $new_path]);
}
  $i = $i + 1;
};
};
  return [];
}
function bidirectional_bfs($start, $goal) {
  global $grid, $delta, $path1, $path2;
  $queue_f = [];
  $queue_b = [];
  $queue_f = _append($queue_f, ['pos' => $start, 'path' => [$start]]);
  $queue_b = _append($queue_b, ['pos' => $goal, 'path' => [$goal]]);
  $head_f = 0;
  $head_b = 0;
  $visited_f = [$start => [$start]];
  $visited_b = [$goal => [$goal]];
  while ($head_f < count($queue_f) && $head_b < count($queue_b)) {
  $node_f = $queue_f[$head_f];
  $head_f = $head_f + 1;
  $neigh_f = neighbors($node_f['pos']);
  $i = 0;
  while ($i < count($neigh_f)) {
  $npos = $neigh_f[$i];
  if (!(array_key_exists($npos, $visited_f))) {
  $new_path = _append($node_f['path'], $npos);
  $visited_f[$npos] = $new_path;
  if (array_key_exists($npos, $visited_b)) {
  $rev = reverse_list($visited_b[$npos]);
  $j = 1;
  while ($j < count($rev)) {
  $new_path = _append($new_path, $rev[$j]);
  $j = $j + 1;
};
  return $new_path;
};
  $queue_f = _append($queue_f, ['pos' => $npos, 'path' => $new_path]);
}
  $i = $i + 1;
};
  $node_b = $queue_b[$head_b];
  $head_b = $head_b + 1;
  $neigh_b = neighbors($node_b['pos']);
  $j = 0;
  while ($j < count($neigh_b)) {
  $nposb = $neigh_b[$j];
  if (!(array_key_exists($nposb, $visited_b))) {
  $new_path_b = _append($node_b['path'], $nposb);
  $visited_b[$nposb] = $new_path_b;
  if (array_key_exists($nposb, $visited_f)) {
  $path_f = $visited_f[$nposb];
  $new_path_b = reverse_list($new_path_b);
  $t = 1;
  while ($t < count($new_path_b)) {
  $path_f = _append($path_f, $new_path_b[$t]);
  $t = $t + 1;
};
  return $path_f;
};
  $queue_b = _append($queue_b, ['pos' => $nposb, 'path' => $new_path_b]);
}
  $j = $j + 1;
};
};
  return [$start];
}
function path_to_string($path) {
  global $grid, $delta, $start, $goal, $path1, $path2;
  if (count($path) == 0) {
  return '[]';
}
  $first = parse_key($path[0]);
  $s = '[(' . _str($first[0]) . ', ' . _str($first[1]) . ')';
  $i = 1;
  while ($i < count($path)) {
  $c = parse_key($path[$i]);
  $s = $s . ', (' . _str($c[0]) . ', ' . _str($c[1]) . ')';
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
$start = mochi_key(0, 0);
$goal = mochi_key(count($grid) - 1, count($grid[0]) - 1);
$path1 = bfs($start, $goal);
echo rtrim(path_to_string($path1)), PHP_EOL;
$path2 = bidirectional_bfs($start, $goal);
echo rtrim(path_to_string($path2)), PHP_EOL;
