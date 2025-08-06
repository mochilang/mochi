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
function new_suffix_tree_node($children, $is_end_of_string, $start, $end, $suffix_link) {
  global $root, $leaf, $nodes, $root_check, $leaf_check;
  return ['children' => $children, 'is_end_of_string' => $is_end_of_string, 'start' => $start, 'end' => $end, 'suffix_link' => $suffix_link];
}
function empty_suffix_tree_node() {
  global $root, $leaf, $nodes, $root_check, $leaf_check;
  return new_suffix_tree_node([], false, 0 - 1, 0 - 1, 0 - 1);
}
function has_key($m, $k) {
  global $root, $leaf, $nodes, $root_check, $leaf_check;
  foreach (array_keys($m) as $key) {
  if ($key == $k) {
  return true;
}
};
  return false;
}
$root = new_suffix_tree_node(['a' => 1], false, 0 - 1, 0 - 1, 0 - 1);
$leaf = new_suffix_tree_node([], true, 0, 2, 0);
$nodes = [$root, $leaf];
$root_check = $nodes[0];
$leaf_check = $nodes[1];
echo rtrim(_str(has_key($root_check['children'], 'a'))), PHP_EOL;
echo rtrim(_str($leaf_check['is_end_of_string'])), PHP_EOL;
echo rtrim(_str($leaf_check['start'])), PHP_EOL;
echo rtrim(_str($leaf_check['end'])), PHP_EOL;
echo rtrim(_str($leaf_check['suffix_link'])), PHP_EOL;
