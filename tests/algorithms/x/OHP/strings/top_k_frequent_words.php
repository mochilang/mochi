<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $freq_map = [];
  function heapify(&$arr, $index, $heap_size) {
  global $freq_map;
  $largest = $index;
  $left = 2 * $index + 1;
  $right = 2 * $index + 2;
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
};
  function build_max_heap(&$arr) {
  global $freq_map;
  $i = count($arr) / 2 - 1;
  while ($i >= 0) {
  heapify($arr, $i, count($arr));
  $i = $i - 1;
};
};
  function top_k_frequent_words($words, $k_value) {
  global $freq_map;
  $freq_map = [];
  $i = 0;
  while ($i < count($words)) {
  $w = $words[$i];
  if (array_key_exists($w, $freq_map)) {
  $freq_map[$w] = $freq_map[$w] + 1;
} else {
  $freq_map[$w] = 1;
}
  $i = $i + 1;
};
  $heap = [];
  foreach (array_keys($freq_map) as $w) {
  $heap = _append($heap, ['word' => $w, 'count' => $freq_map[$w]]);
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
  $heap[0] = $heap[$heap_size - 1];
  $heap[$heap_size - 1] = $item;
  $heap_size = $heap_size - 1;
  heapify($heap, 0, $heap_size);
  $j = $j + 1;
};
  return $result;
};
  function main() {
  global $freq_map;
  $sample = ['a', 'b', 'c', 'a', 'c', 'c'];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 3), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 1), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_k_frequent_words($sample, 0), 1344)))))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
