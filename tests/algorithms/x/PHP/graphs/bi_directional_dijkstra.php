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
function get_min_index($q) {
  global $graph_fwd, $graph_bwd;
  $idx = 0;
  $i = 1;
  while ($i < count($q)) {
  if ($q[$i]['cost'] < $q[$idx]['cost']) {
  $idx = $i;
}
  $i = $i + 1;
};
  return $idx;
}
function remove_at($q, $idx) {
  global $graph_fwd, $graph_bwd;
  $res = [];
  $i = 0;
  while ($i < count($q)) {
  if ($i != $idx) {
  $res = _append($res, $q[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function pass_and_relaxation($graph, $v, $visited_forward, $visited_backward, &$cst_fwd, $cst_bwd, $queue, &$parent, $shortest_distance) {
  global $graph_fwd, $graph_bwd;
  $q = $queue;
  $sd = $shortest_distance;
  foreach ($graph[$v] as $e) {
  $nxt = $e['to'];
  $d = $e['cost'];
  if (array_key_exists($nxt, $visited_forward)) {
  continue;
}
  $old_cost = (array_key_exists($nxt, $cst_fwd) ? $cst_fwd[$nxt] : 2147483647);
  $new_cost = $cst_fwd[$v] + $d;
  if ($new_cost < $old_cost) {
  $q = _append($q, ['node' => $nxt, 'cost' => $new_cost]);
  $cst_fwd[$nxt] = $new_cost;
  $parent[$nxt] = $v;
}
  if (array_key_exists($nxt, $visited_backward)) {
  $alt = $cst_fwd[$v] + $d + $cst_bwd[$nxt];
  if ($alt < $sd) {
  $sd = $alt;
};
}
};
  return ['queue' => $q, 'dist' => $sd];
}
function bidirectional_dij($source, $destination, $graph_forward, $graph_backward) {
  global $graph_fwd, $graph_bwd;
  $shortest_path_distance = -1;
  $visited_forward = [];
  $visited_backward = [];
  $cst_fwd = [];
  $cst_fwd[$source] = 0;
  $cst_bwd = [];
  $cst_bwd[$destination] = 0;
  $parent_forward = [];
  $parent_forward[$source] = '';
  $parent_backward = [];
  $parent_backward[$destination] = '';
  $queue_forward = [];
  $queue_forward = _append($queue_forward, ['node' => $source, 'cost' => 0]);
  $queue_backward = [];
  $queue_backward = _append($queue_backward, ['node' => $destination, 'cost' => 0]);
  $shortest_distance = 2147483647;
  if ($source == $destination) {
  return 0;
}
  while (count($queue_forward) > 0 && count($queue_backward) > 0) {
  $idx_f = get_min_index($queue_forward);
  $item_f = $queue_forward[$idx_f];
  $queue_forward = remove_at($queue_forward, $idx_f);
  $v_fwd = $item_f['node'];
  $visited_forward[$v_fwd] = true;
  $idx_b = get_min_index($queue_backward);
  $item_b = $queue_backward[$idx_b];
  $queue_backward = remove_at($queue_backward, $idx_b);
  $v_bwd = $item_b['node'];
  $visited_backward[$v_bwd] = true;
  $res_f = pass_and_relaxation($graph_forward, $v_fwd, $visited_forward, $visited_backward, $cst_fwd, $cst_bwd, $queue_forward, $parent_forward, $shortest_distance);
  $queue_forward = $res_f['queue'];
  $shortest_distance = $res_f['dist'];
  $res_b = pass_and_relaxation($graph_backward, $v_bwd, $visited_backward, $visited_forward, $cst_bwd, $cst_fwd, $queue_backward, $parent_backward, $shortest_distance);
  $queue_backward = $res_b['queue'];
  $shortest_distance = $res_b['dist'];
  if ($cst_fwd[$v_fwd] + $cst_bwd[$v_bwd] >= $shortest_distance) {
  break;
}
};
  if ($shortest_distance != 2147483647) {
  $shortest_path_distance = $shortest_distance;
}
  return $shortest_path_distance;
}
$graph_fwd = ['B' => [['to' => 'C', 'cost' => 1]], 'C' => [['to' => 'D', 'cost' => 1]], 'D' => [['to' => 'F', 'cost' => 1]], 'E' => [['to' => 'B', 'cost' => 1], ['to' => 'G', 'cost' => 2]], 'F' => [], 'G' => [['to' => 'F', 'cost' => 1]]];
$graph_bwd = ['B' => [['to' => 'E', 'cost' => 1]], 'C' => [['to' => 'B', 'cost' => 1]], 'D' => [['to' => 'C', 'cost' => 1]], 'F' => [['to' => 'D', 'cost' => 1], ['to' => 'G', 'cost' => 1]], 'E' => [], 'G' => [['to' => 'E', 'cost' => 2]]];
echo rtrim(_str(bidirectional_dij('E', 'F', $graph_fwd, $graph_bwd))), PHP_EOL;
