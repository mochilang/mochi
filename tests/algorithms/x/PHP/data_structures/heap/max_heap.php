<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$heap = [0];
$size = 0;
function swap_up($i) {
  global $heap, $size;
  $temp = $heap[$i];
  $idx = $i;
  while (_intdiv($idx, 2) > 0) {
  if ($heap[$idx] > $heap[_intdiv($idx, 2)]) {
  $heap[$idx] = $heap[_intdiv($idx, 2)];
  $heap[_intdiv($idx, 2)] = $temp;
}
  $idx = _intdiv($idx, 2);
};
}
function insert($value) {
  global $heap, $size;
  $heap = _append($heap, $value);
  $size = $size + 1;
  swap_up($size);
}
function swap_down($i) {
  global $heap, $size;
  $idx = $i;
  while ($size >= 2 * $idx) {
  $bigger_child = (2 * $idx + 1 > $size ? 2 * $idx : ($heap[2 * $idx] > $heap[2 * $idx + 1] ? 2 * $idx : 2 * $idx + 1));
  $temp = $heap[$idx];
  if ($heap[$idx] < $heap[$bigger_child]) {
  $heap[$idx] = $heap[$bigger_child];
  $heap[$bigger_child] = $temp;
}
  $idx = $bigger_child;
};
}
function shrink() {
  global $heap, $size;
  $new_heap = [];
  $i = 0;
  while ($i <= $size) {
  $new_heap = _append($new_heap, $heap[$i]);
  $i = $i + 1;
};
  $heap = $new_heap;
}
function pop() {
  global $heap, $size;
  $max_value = $heap[1];
  $heap[1] = $heap[$size];
  $size = $size - 1;
  shrink();
  swap_down(1);
  return $max_value;
}
function get_list() {
  global $heap, $size;
  $out = [];
  $i = 1;
  while ($i <= $size) {
  $out = _append($out, $heap[$i]);
  $i = $i + 1;
};
  return $out;
}
function len() {
  global $heap, $size;
  return $size;
}
insert(6);
insert(10);
insert(15);
insert(12);
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(get_list(), 1344))))))), PHP_EOL;
echo rtrim(json_encode(len(), 1344)), PHP_EOL;
