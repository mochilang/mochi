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
function tarjan($g) {
  $n = count($g);
  $stack = [];
  $on_stack = [];
  $index_of = [];
  $lowlink_of = [];
  $i = 0;
  while ($i < $n) {
  $on_stack = _append($on_stack, false);
  $index_of = _append($index_of, 0 - 1);
  $lowlink_of = _append($lowlink_of, 0 - 1);
  $i = $i + 1;
};
  $components = [];
  $strong_connect = null;
$strong_connect = function($v, $index) use (&$strong_connect, $g, $n, &$stack, &$on_stack, &$index_of, &$lowlink_of, $i, &$components) {
  $index_of[$v] = $index;
  $lowlink_of[$v] = $index;
  $current_index = $index + 1;
  $stack = _append($stack, $v);
  $on_stack[$v] = true;
  foreach ($g[$v] as $w) {
  if ($index_of[$w] == 0 - 1) {
  $current_index = $strong_connect($w, $current_index);
  if ($lowlink_of[$w] < $lowlink_of[$v]) {
  $lowlink_of[$v] = $lowlink_of[$w];
};
} else {
  if ($on_stack[$w]) {
  if ($lowlink_of[$w] < $lowlink_of[$v]) {
  $lowlink_of[$v] = $lowlink_of[$w];
};
};
}
};
  if ($lowlink_of[$v] == $index_of[$v]) {
  $component = [];
  $w = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1);
  $on_stack[$w] = false;
  $component = _append($component, $w);
  while ($w != $v) {
  $w = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1);
  $on_stack[$w] = false;
  $component = _append($component, $w);
};
  $components = _append($components, $component);
}
  return $current_index;
};
  $v = 0;
  while ($v < $n) {
  if ($index_of[$v] == 0 - 1) {
  $strong_connect($v, 0);
}
  $v = $v + 1;
};
  return $components;
}
function create_graph($n, $edges) {
  $g = [];
  $i = 0;
  while ($i < $n) {
  $g = _append($g, []);
  $i = $i + 1;
};
  foreach ($edges as $e) {
  $u = $e[0];
  $v = $e[1];
  $g[$u] = _append($g[$u], $v);
};
  return $g;
}
function main() {
  $n_vertices = 7;
  $source = [0, 0, 1, 2, 3, 3, 4, 4, 6];
  $target = [1, 3, 2, 0, 1, 4, 5, 6, 5];
  $edges = [];
  $i = 0;
  while ($i < count($source)) {
  $edges = _append($edges, [$source[$i], $target[$i]]);
  $i = $i + 1;
};
  $g = create_graph($n_vertices, $edges);
  echo rtrim(_str(tarjan($g))), PHP_EOL;
}
main();
