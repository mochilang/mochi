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
$INF = 1000000000;
function smallest_range($nums) {
  global $INF;
  $heap = [];
  $current_max = -$INF;
  $i = 0;
  while ($i < count($nums)) {
  $first_val = $nums[$i][0];
  $heap = _append($heap, ['value' => $first_val, 'list_idx' => $i, 'elem_idx' => 0]);
  if ($first_val > $current_max) {
  $current_max = $first_val;
}
  $i = $i + 1;
};
  $best = [-$INF, $INF];
  while (count($heap) > 0) {
  $min_idx = 0;
  $j = 1;
  while ($j < count($heap)) {
  $hj = $heap[$j];
  $hmin = $heap[$min_idx];
  if ($hj['value'] < $hmin['value']) {
  $min_idx = $j;
}
  $j = $j + 1;
};
  $item = $heap[$min_idx];
  $new_heap = [];
  $k = 0;
  while ($k < count($heap)) {
  if ($k != $min_idx) {
  $new_heap = _append($new_heap, $heap[$k]);
}
  $k = $k + 1;
};
  $heap = $new_heap;
  $current_min = $item['value'];
  if ($current_max - $current_min < $best[1] - $best[0]) {
  $best = [$current_min, $current_max];
}
  if ($item['elem_idx'] == count($nums[$item['list_idx']]) - 1) {
  break;
}
  $next_val = $nums[$item['list_idx']][$item['elem_idx'] + 1];
  $heap = _append($heap, ['value' => $next_val, 'list_idx' => $item['list_idx'], 'elem_idx' => $item['elem_idx'] + 1]);
  if ($next_val > $current_max) {
  $current_max = $next_val;
}
};
  return $best;
}
function list_to_string($arr) {
  global $INF;
  $s = '[';
  $i = 0;
  while ($i < count($arr)) {
  $s = $s . _str($arr[$i]);
  if ($i < count($arr) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function main() {
  global $INF;
  $result1 = smallest_range([[4, 10, 15, 24, 26], [0, 9, 12, 20], [5, 18, 22, 30]]);
  echo rtrim(list_to_string($result1)), PHP_EOL;
  $result2 = smallest_range([[1, 2, 3], [1, 2, 3], [1, 2, 3]]);
  echo rtrim(list_to_string($result2)), PHP_EOL;
}
main();
