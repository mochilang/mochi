<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function mochi_join($xs) {
  global $G;
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  $i = $i + 1;
};
  return $s;
}
function breadth_first_search($graph, $start) {
  global $G;
  $explored = [];
  $explored[$start] = true;
  $result = [$start];
  $queue = [$start];
  while (count($queue) > 0) {
  $v = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  $children = $graph[$v];
  $i = 0;
  while ($i < count($children)) {
  $w = $children[$i];
  if (!(array_key_exists($w, $explored))) {
  $explored[$w] = true;
  $result = _append($result, $w);
  $queue = _append($queue, $w);
}
  $i = $i + 1;
};
};
  return $result;
}
function breadth_first_search_with_deque($graph, $start) {
  global $G;
  $visited = [];
  $visited[$start] = true;
  $result = [$start];
  $queue = [$start];
  $head = 0;
  while ($head < count($queue)) {
  $v = $queue[$head];
  $head = $head + 1;
  $children = $graph[$v];
  $i = 0;
  while ($i < count($children)) {
  $child = $children[$i];
  if (!(array_key_exists($child, $visited))) {
  $visited[$child] = true;
  $result = _append($result, $child);
  $queue = _append($queue, $child);
}
  $i = $i + 1;
};
};
  return $result;
}
$G = ['A' => ['B', 'C'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F'], 'D' => ['B'], 'E' => ['B', 'F'], 'F' => ['C', 'E']];
echo rtrim(json_encode(mochi_join(breadth_first_search($G, 'A')), 1344)), PHP_EOL;
echo rtrim(json_encode(mochi_join(breadth_first_search_with_deque($G, 'A')), 1344)), PHP_EOL;
