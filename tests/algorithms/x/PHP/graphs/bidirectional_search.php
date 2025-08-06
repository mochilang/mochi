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
function expand_search($graph, $queue, $head, $parents, $visited, $opposite_visited) {
  if ($head >= count($queue)) {
  return ['queue' => $queue, 'head' => $head, 'parents' => $parents, 'visited' => $visited, 'intersection' => 0 - 1, 'found' => false];
}
  $current = $queue[$head];
  $head = $head + 1;
  $neighbors = $graph[$current];
  $q = $queue;
  $p = $parents;
  $v = $visited;
  $i = 0;
  while ($i < count($neighbors)) {
  $neighbor = $neighbors[$i];
  if ($v[$neighbor]) {
  $i = $i + 1;
  continue;
}
  $v[$neighbor] = true;
  $p[$neighbor] = $current;
  $q = _append($q, $neighbor);
  if ($opposite_visited[$neighbor]) {
  return ['queue' => $q, 'head' => $head, 'parents' => $p, 'visited' => $v, 'intersection' => $neighbor, 'found' => true];
}
  $i = $i + 1;
};
  return ['queue' => $q, 'head' => $head, 'parents' => $p, 'visited' => $v, 'intersection' => 0 - 1, 'found' => false];
}
function construct_path($current, $parents) {
  $path = [];
  $node = $current;
  while ($node != 0 - 1) {
  $path = _append($path, $node);
  $node = $parents[$node];
};
  return $path;
}
function reverse_list($xs) {
  $res = [];
  $i = count($xs);
  while ($i > 0) {
  $i = $i - 1;
  $res = _append($res, $xs[$i]);
};
  return $res;
}
function bidirectional_search($g, $start, $goal) {
  if ($start == $goal) {
  return ['path' => [$start], 'ok' => true];
}
  $forward_parents = [];
  $forward_parents[$start] = 0 - 1;
  $backward_parents = [];
  $backward_parents[$goal] = 0 - 1;
  $forward_visited = [];
  $forward_visited[$start] = true;
  $backward_visited = [];
  $backward_visited[$goal] = true;
  $forward_queue = [$start];
  $backward_queue = [$goal];
  $forward_head = 0;
  $backward_head = 0;
  $intersection = 0 - 1;
  while ($forward_head < count($forward_queue) && $backward_head < count($backward_queue) && $intersection == 0 - 1) {
  $res = expand_search($g, $forward_queue, $forward_head, $forward_parents, $forward_visited, $backward_visited);
  $forward_queue = $res['queue'];
  $forward_head = $res['head'];
  $forward_parents = $res['parents'];
  $forward_visited = $res['visited'];
  if ($res['found']) {
  $intersection = $res['intersection'];
  break;
}
  $res = expand_search($g, $backward_queue, $backward_head, $backward_parents, $backward_visited, $forward_visited);
  $backward_queue = $res['queue'];
  $backward_head = $res['head'];
  $backward_parents = $res['parents'];
  $backward_visited = $res['visited'];
  if ($res['found']) {
  $intersection = $res['intersection'];
  break;
}
};
  if ($intersection == 0 - 1) {
  return ['path' => [], 'ok' => false];
}
  $forward_path = construct_path($intersection, $forward_parents);
  $forward_path = reverse_list($forward_path);
  $back_start = $backward_parents[$intersection];
  $backward_path = construct_path($back_start, $backward_parents);
  $result = $forward_path;
  $j = 0;
  while ($j < count($backward_path)) {
  $result = _append($result, $backward_path[$j]);
  $j = $j + 1;
};
  return ['path' => $result, 'ok' => true];
}
function is_edge($g, $u, $v) {
  $neighbors = $g[$u];
  $i = 0;
  while ($i < count($neighbors)) {
  if ($neighbors[$i] == $v) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function path_exists($g, $path) {
  if (count($path) == 0) {
  return false;
}
  $i = 0;
  while ($i + 1 < count($path)) {
  if (!is_edge($g, $path[$i], $path[$i + 1])) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function print_path($g, $s, $t) {
  $res = bidirectional_search($g, $s, $t);
  if ($res['ok'] && path_exists($g, $res['path'])) {
  echo rtrim('Path from ' . _str($s) . ' to ' . _str($t) . ': ' . _str($res['path'])), PHP_EOL;
} else {
  echo rtrim('Path from ' . _str($s) . ' to ' . _str($t) . ': None'), PHP_EOL;
}
}
function main() {
  $graph = [0 => [1, 2], 1 => [0, 3, 4], 2 => [0, 5, 6], 3 => [1, 7], 4 => [1, 8], 5 => [2, 9], 6 => [2, 10], 7 => [3, 11], 8 => [4, 11], 9 => [5, 11], 10 => [6, 11], 11 => [7, 8, 9, 10]];
  print_path($graph, 0, 11);
  print_path($graph, 5, 5);
  $disconnected = [0 => [1, 2], 1 => [0], 2 => [0], 3 => [4], 4 => [3]];
  print_path($disconnected, 0, 3);
}
main();
