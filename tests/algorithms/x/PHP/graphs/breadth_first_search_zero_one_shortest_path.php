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
function new_adjacency_list($size) {
  $g = [];
  $i = 0;
  while ($i < $size) {
  $g = _append($g, []);
  $i = $i + 1;
};
  return ['graph' => $g, 'size' => $size];
}
function add_edge(&$al, $from_vertex, $to_vertex, $weight) {
  if (!($weight == 0 || $weight == 1)) {
  $panic('Edge weight must be either 0 or 1.');
}
  if ($to_vertex < 0 || $to_vertex >= $al['size']) {
  $panic('Vertex indexes must be in [0; size).');
}
  $g = $al['graph'];
  $edges = $g[$from_vertex];
  $g[$from_vertex] = _append($edges, ['destination_vertex' => $to_vertex, $weight => $weight]);
  $al['graph'] = $g;
}
function push_front($q, $v) {
  global $g;
  $res = [$v];
  $i = 0;
  while ($i < count($q)) {
  $res = _append($res, $q[$i]);
  $i = $i + 1;
};
  return $res;
}
function pop_front($q) {
  global $g;
  $res = [];
  $i = 1;
  while ($i < count($q)) {
  $res = _append($res, $q[$i]);
  $i = $i + 1;
};
  return $res;
}
function front($q) {
  global $g;
  return $q[0];
}
function get_shortest_path($al, $start_vertex, $finish_vertex) {
  global $g;
  $queue = [$start_vertex];
  $distances = [];
  $i = 0;
  while ($i < $al['size']) {
  $distances = _append($distances, -1);
  $i = $i + 1;
};
  $distances[$start_vertex] = 0;
  while (count($queue) > 0) {
  $current_vertex = front($queue);
  $queue = pop_front($queue);
  $current_distance = $distances[$current_vertex];
  $edges = $al['graph'][$current_vertex];
  $j = 0;
  while ($j < count($edges)) {
  $edge = $edges[$j];
  $new_distance = $current_distance + $edge['weight'];
  $dest = $edge['destination_vertex'];
  $dest_distance = $distances[$dest];
  if ($dest_distance >= 0 && $new_distance >= $dest_distance) {
  $j = $j + 1;
  continue;
}
  $distances[$dest] = $new_distance;
  if ($edge['weight'] == 0) {
  $queue = push_front($queue, $dest);
} else {
  $queue = _append($queue, $dest);
}
  $j = $j + 1;
};
};
  $result = $distances[$finish_vertex];
  if ($result < 0) {
  $panic('No path from start_vertex to finish_vertex.');
}
  return $result;
}
$g = new_adjacency_list(11);
add_edge($g, 0, 1, 0);
add_edge($g, 0, 3, 1);
add_edge($g, 1, 2, 0);
add_edge($g, 2, 3, 0);
add_edge($g, 4, 2, 1);
add_edge($g, 4, 5, 1);
add_edge($g, 4, 6, 1);
add_edge($g, 5, 9, 0);
add_edge($g, 6, 7, 1);
add_edge($g, 7, 8, 1);
add_edge($g, 8, 10, 1);
add_edge($g, 9, 7, 0);
add_edge($g, 9, 10, 1);
echo rtrim(_str(get_shortest_path($g, 0, 3))), PHP_EOL;
echo rtrim(_str(get_shortest_path($g, 4, 10))), PHP_EOL;
echo rtrim(_str(get_shortest_path($g, 4, 8))), PHP_EOL;
echo rtrim(_str(get_shortest_path($g, 0, 1))), PHP_EOL;
