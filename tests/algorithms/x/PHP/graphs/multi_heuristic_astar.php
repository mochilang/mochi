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
$W1 = 1.0;
$W2 = 1.0;
$n = 20;
$n_heuristic = 3;
$INF = 1000000000.0;
$t = 1;
function pos_equal($a, $b) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  return $a['x'] == $b['x'] && $a['y'] == $b['y'];
}
function pos_key($p) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  return _str($p['x']) . ',' . _str($p['y']);
}
function sqrtApprox($x) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
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
function consistent_heuristic($p, $goal) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start;
  $dx = floatval(($p['x'] - $goal['x']));
  $dy = floatval(($p['y'] - $goal['y']));
  return sqrtApprox($dx * $dx + $dy * $dy);
}
function iabs($x) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function heuristic_1($p, $goal) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start;
  return floatval((iabs($p['x'] - $goal['x']) + iabs($p['y'] - $goal['y'])));
}
function heuristic_2($p, $goal) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start;
  $h = consistent_heuristic($p, $goal);
  return $h / (floatval($t));
}
function heuristic($i, $p, $goal) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start;
  if ($i == 0) {
  return consistent_heuristic($p, $goal);
}
  if ($i == 1) {
  return heuristic_1($p, $goal);
}
  return heuristic_2($p, $goal);
}
function key_fn($start, $i, $goal, $g_func) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks;
  $g = $g_func[pos_key($start)];
  return $g + $W1 * heuristic($i, $start, $goal);
}
function valid($p) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  if ($p['x'] < 0 || $p['x'] > $n - 1) {
  return false;
}
  if ($p['y'] < 0 || $p['y'] > $n - 1) {
  return false;
}
  return true;
}
$blocks = [['x' => 0, 'y' => 1], ['x' => 1, 'y' => 1], ['x' => 2, 'y' => 1], ['x' => 3, 'y' => 1], ['x' => 4, 'y' => 1], ['x' => 5, 'y' => 1], ['x' => 6, 'y' => 1], ['x' => 7, 'y' => 1], ['x' => 8, 'y' => 1], ['x' => 9, 'y' => 1], ['x' => 10, 'y' => 1], ['x' => 11, 'y' => 1], ['x' => 12, 'y' => 1], ['x' => 13, 'y' => 1], ['x' => 14, 'y' => 1], ['x' => 15, 'y' => 1], ['x' => 16, 'y' => 1], ['x' => 17, 'y' => 1], ['x' => 18, 'y' => 1], ['x' => 19, 'y' => 1]];
function in_blocks($p) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  $i = 0;
  while ($i < count($blocks)) {
  if (pos_equal($blocks[$i], $p)) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function pq_put(&$pq, $node, $pri) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  $updated = false;
  $i = 0;
  while ($i < count($pq)) {
  if (pos_equal($pq[$i]['pos'], $node)) {
  if ($pri < $pq[$i]['pri']) {
  $pq[$i] = ['pos' => $node, 'pri' => $pri];
};
  $updated = true;
}
  $i = $i + 1;
};
  if (!$updated) {
  $pq = _append($pq, ['pos' => $node, $pri => $pri]);
}
  return $pq;
}
function pq_minkey($pq) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  if (count($pq) == 0) {
  return $INF;
}
  $first = $pq[0];
  $m = $first['pri'];
  $i = 1;
  while ($i < count($pq)) {
  $item = $pq[$i];
  if ($item['pri'] < $m) {
  $m = $item['pri'];
}
  $i = $i + 1;
};
  return $m;
}
function pq_pop_min($pq) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  $best = $pq[0];
  $idx = 0;
  $i = 1;
  while ($i < count($pq)) {
  if ($pq[$i]['pri'] < $best['pri']) {
  $best = $pq[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $new_pq = [];
  $i = 0;
  while ($i < count($pq)) {
  if ($i != $idx) {
  $new_pq = _append($new_pq, $pq[$i]);
}
  $i = $i + 1;
};
  return ['pq' => $new_pq, 'node' => $best];
}
function pq_remove($pq, $node) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  $new_pq = [];
  $i = 0;
  while ($i < count($pq)) {
  if (!pos_equal($pq[$i]['pos'], $node)) {
  $new_pq = _append($new_pq, $pq[$i]);
}
  $i = $i + 1;
};
  return $new_pq;
}
function reconstruct($back_pointer, $goal, $start) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks;
  $path = [];
  $current = $goal;
  $key = pos_key($current);
  $path = _append($path, $current);
  while (!(pos_equal($current, $start))) {
  $current = $back_pointer[$key];
  $key = pos_key($current);
  $path = _append($path, $current);
};
  $rev = [];
  $i = count($path) - 1;
  while ($i >= 0) {
  $rev = _append($rev, $path[$i]);
  $i = $i - 1;
};
  return $rev;
}
function neighbours($p) {
  global $W1, $W2, $n, $n_heuristic, $INF, $t, $blocks, $start, $goal;
  $left = ['x' => $p['x'] - 1, 'y' => $p['y']];
  $right = ['x' => $p['x'] + 1, 'y' => $p['y']];
  $up = ['x' => $p['x'], 'y' => $p['y'] + 1];
  $down = ['x' => $p['x'], 'y' => $p['y'] - 1];
  return [$left, $right, $up, $down];
}
function multi_a_star($start, $goal, $n_heuristic) {
  global $W1, $W2, $n, $INF, $t, $blocks;
  $g_function = [];
  $back_pointer = [];
  $visited = [];
  $open_list = [];
  $g_function[pos_key($start)] = 0.0;
  $g_function[pos_key($goal)] = $INF;
  $back_pointer[pos_key($start)] = ['x' => -1, 'y' => -1];
  $back_pointer[pos_key($goal)] = ['x' => -1, 'y' => -1];
  $visited[pos_key($start)] = true;
  $i = 0;
  while ($i < $n_heuristic) {
  $open_list = _append($open_list, []);
  $pri = key_fn($start, $i, $goal, $g_function);
  $open_list[$i] = pq_put($open_list[$i], $start, $pri);
  $i = $i + 1;
};
  while (pq_minkey($open_list[0]) < $INF) {
  $chosen = 0;
  $i = 1;
  while ($i < $n_heuristic) {
  if (pq_minkey($open_list[$i]) <= $W2 * pq_minkey($open_list[0])) {
  $chosen = $i;
  break;
}
  $i = $i + 1;
};
  if ($chosen != 0) {
  $t = $t + 1;
}
  $pair = pq_pop_min($open_list[$chosen]);
  $open_list[$chosen] = $pair['pq'];
  $current = $pair['node'];
  $i = 0;
  while ($i < $n_heuristic) {
  if ($i != $chosen) {
  $open_list[$i] = pq_remove($open_list[$i], $current['pos']);
}
  $i = $i + 1;
};
  $ckey = pos_key($current['pos']);
  if (array_key_exists($ckey, $visited)) {
  continue;
}
  $visited[$ckey] = true;
  if (pos_equal($current['pos'], $goal)) {
  $path = reconstruct($back_pointer, $goal, $start);
  $j = 0;
  while ($j < count($path)) {
  $p = $path[$j];
  echo rtrim('(' . _str($p['x']) . ',' . _str($p['y']) . ')'), PHP_EOL;
  $j = $j + 1;
};
  return;
}
  $neighs = neighbours($current['pos']);
  $k = 0;
  while ($k < count($neighs)) {
  $nb = $neighs[$k];
  if (valid($nb) && (in_blocks($nb) == false)) {
  $nkey = pos_key($nb);
  $tentative = $g_function[$ckey] + 1.0;
  if (!(array_key_exists($nkey, $g_function)) || $tentative < $g_function[$nkey]) {
  $g_function[$nkey] = $tentative;
  $back_pointer[$nkey] = $current['pos'];
  $i = 0;
  while ($i < $n_heuristic) {
  $pri2 = $tentative + $W1 * heuristic($i, $nb, $goal);
  $open_list[$i] = pq_put($open_list[$i], $nb, $pri2);
  $i = $i + 1;
};
};
}
  $k = $k + 1;
};
};
  echo rtrim('No path found to goal'), PHP_EOL;
}
$start = ['x' => 0, 'y' => 0];
$goal = ['x' => $n - 1, 'y' => $n - 1];
multi_a_star($start, $goal, $n_heuristic);
