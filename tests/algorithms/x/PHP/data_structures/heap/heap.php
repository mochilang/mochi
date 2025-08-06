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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function parent_index($child_idx) {
  global $heap, $size, $m;
  if ($child_idx > 0) {
  return _intdiv(($child_idx - 1), 2);
}
  return -1;
}
function left_child_idx($parent_idx) {
  global $heap, $size, $m;
  return 2 * $parent_idx + 1;
}
function right_child_idx($parent_idx) {
  global $heap, $size, $m;
  return 2 * $parent_idx + 2;
}
function max_heapify(&$h, $heap_size, $index) {
  global $heap, $size, $m;
  $largest = $index;
  $left = left_child_idx($index);
  $right = right_child_idx($index);
  if ($left < $heap_size && $h[$left] > $h[$largest]) {
  $largest = $left;
}
  if ($right < $heap_size && $h[$right] > $h[$largest]) {
  $largest = $right;
}
  if ($largest != $index) {
  $temp = $h[$index];
  $h[$index] = $h[$largest];
  $h[$largest] = $temp;
  max_heapify($h, $heap_size, $largest);
}
}
function build_max_heap(&$h) {
  global $heap, $size, $m;
  $heap_size = count($h);
  $i = _intdiv($heap_size, 2) - 1;
  while ($i >= 0) {
  max_heapify($h, $heap_size, $i);
  $i = $i - 1;
};
  return $heap_size;
}
function extract_max(&$h, $heap_size) {
  global $heap, $size, $m;
  $max_value = $h[0];
  $h[0] = $h[$heap_size - 1];
  max_heapify($h, $heap_size - 1, 0);
  return $max_value;
}
function insert(&$h, $heap_size, $value) {
  global $heap, $size, $m;
  if ($heap_size < count($h)) {
  $h[$heap_size] = $value;
} else {
  $h = _append($h, $value);
}
  $heap_size = $heap_size + 1;
  $idx = _intdiv(($heap_size - 1), 2);
  while ($idx >= 0) {
  max_heapify($h, $heap_size, $idx);
  $idx = _intdiv(($idx - 1), 2);
};
  return $heap_size;
}
function heap_sort(&$h, $heap_size) {
  global $heap, $m;
  $size = $heap_size;
  $j = $size - 1;
  while ($j > 0) {
  $temp = $h[0];
  $h[0] = $h[$j];
  $h[$j] = $temp;
  $size = $size - 1;
  max_heapify($h, $size, 0);
  $j = $j - 1;
};
}
function heap_to_string($h, $heap_size) {
  global $heap, $size, $m;
  $s = '[';
  $i = 0;
  while ($i < $heap_size) {
  $s = $s . _str($h[$i]);
  if ($i < $heap_size - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
$heap = [103.0, 9.0, 1.0, 7.0, 11.0, 15.0, 25.0, 201.0, 209.0, 107.0, 5.0];
$size = build_max_heap($heap);
echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
$m = extract_max($heap, $size);
$size = $size - 1;
echo rtrim(_str($m)), PHP_EOL;
echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
$size = insert($heap, $size, 100.0);
echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
heap_sort($heap, $size);
echo rtrim(heap_to_string($heap, $size)), PHP_EOL;
