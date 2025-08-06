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
function add_edge(&$graph, $from, $to) {
  global $g;
  if (array_key_exists($from, $graph)) {
  $graph[$from] = _append($graph[$from], $to);
} else {
  $graph[$from] = [$to];
}
}
function print_graph($graph) {
  global $g;
  foreach (array_keys($graph) as $v) {
  $adj = $graph[$v];
  $line = _str($v) . '  :  ';
  $i = 0;
  while ($i < count($adj)) {
  $line = $line . _str($adj[$i]);
  if ($i < count($adj) - 1) {
  $line = $line . ' -> ';
}
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
};
}
function bfs($graph, $start) {
  global $g;
  $visited = [];
  $queue = [];
  $order = [];
  $queue = _append($queue, $start);
  $visited[$start] = true;
  $head = 0;
  while ($head < count($queue)) {
  $vertex = $queue[$head];
  $head = $head + 1;
  $order = _append($order, $vertex);
  $neighbors = $graph[$vertex];
  $i = 0;
  while ($i < count($neighbors)) {
  $neighbor = $neighbors[$i];
  if (!(array_key_exists($neighbor, $visited))) {
  $visited[$neighbor] = true;
  $queue = _append($queue, $neighbor);
}
  $i = $i + 1;
};
};
  return $order;
}
$g = [];
add_edge($g, 0, 1);
add_edge($g, 0, 2);
add_edge($g, 1, 2);
add_edge($g, 2, 0);
add_edge($g, 2, 3);
add_edge($g, 3, 3);
print_graph($g);
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(bfs($g, 2), 1344)))))), PHP_EOL;
