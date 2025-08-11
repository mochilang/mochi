<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$freq_map = [];
function heapify(&$arr, $index, $heap_size) {
  global $freq_map;
  $largest = $index;
  $left = _iadd(_imul(2, $index), 1);
  $right = _iadd(_imul(2, $index), 2);
  if ($left < $heap_size) {
  $left_item = $arr[$left];
  $largest_item = $arr[$largest];
  if ($left_item['count'] > $largest_item['count']) {
  $largest = $left;
};
}
  if ($right < $heap_size) {
  $right_item = $arr[$right];
  $largest_item2 = $arr[$largest];
  if ($right_item['count'] > $largest_item2['count']) {
  $largest = $right;
};
}
  if ($largest != $index) {
  $temp = $arr[$largest];
  $arr[$largest] = $arr[$index];
  $arr[$index] = $temp;
  heapify($arr, $largest, $heap_size);
}
}
function build_max_heap(&$arr) {
  global $freq_map;
  $i = _isub(_idiv(count($arr), 2), 1);
  while ($i >= 0) {
  heapify($arr, $i, count($arr));
  $i = _isub($i, 1);
};
}
function top_k_frequent_words($words, $k_value) {
  global $freq_map;
  $freq_map = [];
  $i = 0;
  while ($i < count($words)) {
  $w = $words[$i];
  if (array_key_exists($w, $freq_map)) {
  $freq_map[$w] = _iadd($freq_map[$w], 1);
} else {
  $freq_map[$w] = 1;
}
  $i = _iadd($i, 1);
};
  $heap = [];
  foreach (array_keys($freq_map) as $w) {
  $heap = _append($heap, ['word' => $w, $count => $freq_map[$w]]);
};
  build_max_heap($heap);
  $result = [];
  $heap_size = count($heap);
  $limit = $k_value;
  if ($limit > $heap_size) {
  $limit = $heap_size;
}
  $j = 0;
  while ($j < $limit) {
  $item = $heap[0];
  $result = _append($result, $item['word']);
  $heap[0] = $heap[_isub($heap_size, 1)];
  $heap[_isub($heap_size, 1)] = $item;
  $heap_size = _isub($heap_size, 1);
  heapify($heap, 0, $heap_size);
  $j = _iadd($j, 1);
};
  return $result;
}
function main() {
  global $freq_map;
  $sample = ['a', 'b', 'c', 'a', 'c', 'c'];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 3), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 1), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 0), 1344)))))), PHP_EOL;
}
main();
