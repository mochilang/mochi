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
function is_bipartite_bfs($graph) {
  $visited = [];
  foreach (array_keys($graph) as $node) {
  if (!(array_key_exists($node, $visited))) {
  $queue = [];
  $queue = _append($queue, $node);
  $visited[$node] = 0;
  while (count($queue) > 0) {
  $curr = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  foreach ($graph[$curr] as $neighbor) {
  if (!(array_key_exists($neighbor, $visited))) {
  $visited[$neighbor] = 1 - $visited[$curr];
  $queue = _append($queue, $neighbor);
} else {
  if ($visited[$neighbor] == $visited[$curr]) {
  return false;
};
}
};
};
}
};
  return true;
}
$graph = [0 => [1, 3], 1 => [0, 2], 2 => [1, 3], 3 => [0, 2]];
echo rtrim(_str(is_bipartite_bfs($graph))), PHP_EOL;
