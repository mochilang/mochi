<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
$HEURISTIC = 0;
$grid = [[0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0], [1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 0, 0]];
$delta = [[-1, 0], [0, -1], [1, 0], [0, 1]];
function mochi_abs($x) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function heuristic($a, $b) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $dy = $a['y'] - $b['y'];
  $dx = $a['x'] - $b['x'];
  if ($HEURISTIC == 1) {
  return floatval((mochi_abs($dy) + mochi_abs($dx)));
}
  $dyf = (floatval($dy));
  $dxf = (floatval($dx));
  return sqrtApprox($dyf * $dyf + $dxf * $dxf);
}
function pos_equal($a, $b) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  return $a['y'] == $b['y'] && $a['x'] == $b['x'];
}
function contains_pos($lst, $p) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $i = 0;
  while ($i < count($lst)) {
  if (pos_equal($lst[$i], $p)) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function open_index_of_pos($open, $p) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $i = 0;
  while ($i < count($open)) {
  if (pos_equal($open[$i]['pos'], $p)) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
}
function remove_node_at($nodes, $idx) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = 0;
  while ($i < count($nodes)) {
  if ($i != $idx) {
  $res = _append($res, $nodes[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function append_pos_list($path, $p) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = 0;
  while ($i < count($path)) {
  $res = _append($res, $path[$i]);
  $i = $i + 1;
};
  $res = _append($res, $p);
  return $res;
}
function reverse_pos_list($lst) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = count($lst) - 1;
  while ($i >= 0) {
  $res = _append($res, $lst[$i]);
  $i = $i - 1;
};
  return $res;
}
function concat_pos_lists($a, $b) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($b)) {
  $res = _append($res, $b[$j]);
  $j = $j + 1;
};
  return $res;
}
function get_successors($p) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $res = [];
  $i = 0;
  while ($i < count($delta)) {
  $nx = $p['x'] + $delta[$i][1];
  $ny = $p['y'] + $delta[$i][0];
  if ($nx >= 0 && $ny >= 0 && $nx < count($grid[0]) && $ny < count($grid)) {
  if ($grid[$ny][$nx] == 0) {
  $res = _append($res, ['y' => $ny, 'x' => $nx]);
};
}
  $i = $i + 1;
};
  return $res;
}
function find_lowest_f($open) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $best = 0;
  $i = 1;
  while ($i < count($open)) {
  if ($open[$i]['f_cost'] < $open[$best]['f_cost']) {
  $best = $i;
}
  $i = $i + 1;
};
  return $best;
}
function astar($start, $goal) {
  global $HEURISTIC, $grid, $delta, $path1, $path2;
  $h0 = heuristic($start, $goal);
  $open = [['pos' => $start, 'g_cost' => 0, 'h_cost' => $h0, 'f_cost' => $h0, 'path' => [$start]]];
  $closed = [];
  while (count($open) > 0) {
  $idx = find_lowest_f($open);
  $current = $open[$idx];
  $open = remove_node_at($open, $idx);
  if (pos_equal($current['pos'], $goal)) {
  return $current['path'];
}
  $closed = _append($closed, $current['pos']);
  $succ = get_successors($current['pos']);
  $i = 0;
  while ($i < count($succ)) {
  $pos = $succ[$i];
  if (contains_pos($closed, $pos)) {
  $i = $i + 1;
  continue;
}
  $tentative_g = $current['g_cost'] + 1;
  $idx_open = open_index_of_pos($open, $pos);
  if ($idx_open == 0 - 1 || $tentative_g < $open[$idx_open]['g_cost']) {
  $new_path = append_pos_list($current['path'], $pos);
  $h = heuristic($pos, $goal);
  $f = (floatval($tentative_g)) + $h;
  if ($idx_open != 0 - 1) {
  $open = remove_node_at($open, $idx_open);
};
  $open = _append($open, [$pos => $pos, 'g_cost' => $tentative_g, 'h_cost' => $h, 'f_cost' => $f, 'path' => $new_path]);
}
  $i = $i + 1;
};
};
  return [$start];
}
function combine_paths($fwd, $bwd) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  $bwd_copy = [];
  $i = 0;
  while ($i < _len($bwd['path']) - 1) {
  $bwd_copy = _append($bwd_copy, $bwd['path'][$i]);
  $i = $i + 1;
};
  $bwd_copy = reverse_pos_list($bwd_copy);
  return concat_pos_lists($fwd['path'], $bwd_copy);
}
function bidirectional_astar($start, $goal) {
  global $HEURISTIC, $grid, $delta, $path1, $path2;
  $hf = heuristic($start, $goal);
  $hb = heuristic($goal, $start);
  $open_f = [['pos' => $start, 'g_cost' => 0, 'h_cost' => $hf, 'f_cost' => $hf, 'path' => [$start]]];
  $open_b = [['pos' => $goal, 'g_cost' => 0, 'h_cost' => $hb, 'f_cost' => $hb, 'path' => [$goal]]];
  $closed_f = [];
  $closed_b = [];
  while (count($open_f) > 0 && count($open_b) > 0) {
  $idx_f = find_lowest_f($open_f);
  $current_f = $open_f[$idx_f];
  $open_f = remove_node_at($open_f, $idx_f);
  $idx_b = find_lowest_f($open_b);
  $current_b = $open_b[$idx_b];
  $open_b = remove_node_at($open_b, $idx_b);
  if (pos_equal($current_f['pos'], $current_b['pos'])) {
  return combine_paths($current_f, $current_b);
}
  $closed_f = _append($closed_f, $current_f['pos']);
  $closed_b = _append($closed_b, $current_b['pos']);
  $succ_f = get_successors($current_f['pos']);
  $i = 0;
  while ($i < count($succ_f)) {
  $pos = $succ_f[$i];
  if (contains_pos($closed_f, $pos)) {
  $i = $i + 1;
  continue;
}
  $tentative_g = $current_f['g_cost'] + 1;
  $h = heuristic($pos, $current_b['pos']);
  $f = (floatval($tentative_g)) + $h;
  $idx_open = open_index_of_pos($open_f, $pos);
  if ($idx_open == 0 - 1 || $tentative_g < $open_f[$idx_open]['g_cost']) {
  $new_path = append_pos_list($current_f['path'], $pos);
  if ($idx_open != 0 - 1) {
  $open_f = remove_node_at($open_f, $idx_open);
};
  $open_f = _append($open_f, [$pos => $pos, 'g_cost' => $tentative_g, 'h_cost' => $h, 'f_cost' => $f, 'path' => $new_path]);
}
  $i = $i + 1;
};
  $succ_b = get_successors($current_b['pos']);
  $i = 0;
  while ($i < count($succ_b)) {
  $pos = $succ_b[$i];
  if (contains_pos($closed_b, $pos)) {
  $i = $i + 1;
  continue;
}
  $tentative_g = $current_b['g_cost'] + 1;
  $h = heuristic($pos, $current_f['pos']);
  $f = (floatval($tentative_g)) + $h;
  $idx_open = open_index_of_pos($open_b, $pos);
  if ($idx_open == 0 - 1 || $tentative_g < $open_b[$idx_open]['g_cost']) {
  $new_path = append_pos_list($current_b['path'], $pos);
  if ($idx_open != 0 - 1) {
  $open_b = remove_node_at($open_b, $idx_open);
};
  $open_b = _append($open_b, [$pos => $pos, 'g_cost' => $tentative_g, 'h_cost' => $h, 'f_cost' => $f, 'path' => $new_path]);
}
  $i = $i + 1;
};
};
  return [$start];
}
function path_to_string($path) {
  global $HEURISTIC, $grid, $delta, $start, $goal, $path1, $path2;
  if (count($path) == 0) {
  return '[]';
}
  $s = '[(' . _str($path[0]['y']) . ', ' . _str($path[0]['x']) . ')';
  $i = 1;
  while ($i < count($path)) {
  $s = $s . ', (' . _str($path[$i]['y']) . ', ' . _str($path[$i]['x']) . ')';
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
$start = ['y' => 0, 'x' => 0];
$goal = ['y' => count($grid) - 1, 'x' => count($grid[0]) - 1];
$path1 = astar($start, $goal);
echo rtrim(path_to_string($path1)), PHP_EOL;
$path2 = bidirectional_astar($start, $goal);
echo rtrim(path_to_string($path2)), PHP_EOL;
