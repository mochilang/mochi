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
function repeat_bool($times) {
  global $sequence, $sequence_2;
  $res = [];
  $i = 0;
  while ($i < $times) {
  $res = _append($res, false);
  $i = $i + 1;
};
  return $res;
}
function set_bool($xs, $idx, $value) {
  global $sequence, $sequence_2;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function create_state_space_tree($sequence, $current, $index, $used) {
  global $sequence_2;
  if ($index == count($sequence)) {
  echo rtrim(_str($current)), PHP_EOL;
  return;
}
  $i = 0;
  while ($i < count($sequence)) {
  if (!$used[$i]) {
  $next_current = _append($current, $sequence[$i]);
  $next_used = set_bool($used, $i, true);
  create_state_space_tree($sequence, $next_current, $index + 1, $next_used);
}
  $i = $i + 1;
};
}
function generate_all_permutations($sequence) {
  global $sequence_2;
  $used = repeat_bool(count($sequence));
  create_state_space_tree($sequence, [], 0, $used);
}
$sequence = [3, 1, 2, 4];
generate_all_permutations($sequence);
$sequence_2 = ['A', 'B', 'C'];
generate_all_permutations($sequence_2);
