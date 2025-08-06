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
function make_kd_node($point, $left, $right) {
  global $nodes, $root, $left_child, $right_child;
  return ['point' => $point, 'left' => $left, 'right' => $right];
}
$nodes = [];
$nodes = _append($nodes, make_kd_node([2.0, 3.0], 1, 2));
$nodes = _append($nodes, make_kd_node([1.0, 5.0], -1, -1));
$nodes = _append($nodes, make_kd_node([4.0, 2.0], -1, -1));
$root = $nodes[0];
$left_child = $nodes[1];
$right_child = $nodes[2];
echo rtrim(_str($root['point'])), PHP_EOL;
echo rtrim(_str($root['left'])), PHP_EOL;
echo rtrim(_str($root['right'])), PHP_EOL;
echo rtrim(_str($left_child['point'])), PHP_EOL;
echo rtrim(_str($right_child['point'])), PHP_EOL;
