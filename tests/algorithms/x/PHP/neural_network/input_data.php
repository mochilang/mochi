<?php
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
$__start_mem = memory_get_usage();
$__start = _now();
  function dense_to_one_hot($labels, $num_classes) {
  $result = [];
  $i = 0;
  while ($i < count($labels)) {
  $row = [];
  $j = 0;
  while ($j < $num_classes) {
  if ($j == $labels[$i]) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function new_dataset($images, $labels) {
  return ['images' => $images, 'labels' => $labels, 'num_examples' => count($images), 'index_in_epoch' => 0, 'epochs_completed' => 0];
};
  function next_batch($ds, $batch_size) {
  $start = $ds['index_in_epoch'];
  if ($start + $batch_size > $ds['num_examples']) {
  $rest = $ds['num_examples'] - $start;
  $images_rest = array_slice($ds['images'], $start, $ds['num_examples'] - $start);
  $labels_rest = array_slice($ds['labels'], $start, $ds['num_examples'] - $start);
  $new_index = $batch_size - $rest;
  $images_new = array_slice($ds['images'], 0, $new_index - 0);
  $labels_new = array_slice($ds['labels'], 0, $new_index - 0);
  $batch_images = array_merge($images_rest, $images_new);
  $batch_labels = array_merge($labels_rest, $labels_new);
  $new_ds = ['images' => $ds['images'], 'labels' => $ds['labels'], 'num_examples' => $ds['num_examples'], 'index_in_epoch' => $new_index, 'epochs_completed' => $ds['epochs_completed'] + 1];
  return ['dataset' => $new_ds, 'images' => $batch_images, 'labels' => $batch_labels];
} else {
  $end = $start + $batch_size;
  $batch_images = array_slice($ds['images'], $start, $end - $start);
  $batch_labels = array_slice($ds['labels'], $start, $end - $start);
  $new_ds = ['images' => $ds['images'], 'labels' => $ds['labels'], 'num_examples' => $ds['num_examples'], 'index_in_epoch' => $end, 'epochs_completed' => $ds['epochs_completed']];
  return ['dataset' => $new_ds, 'images' => $batch_images, 'labels' => $batch_labels];
}
};
  function read_data_sets($train_images, $train_labels_raw, $test_images, $test_labels_raw, $validation_size, $num_classes) {
  $train_labels = dense_to_one_hot($train_labels_raw, $num_classes);
  $test_labels = dense_to_one_hot($test_labels_raw, $num_classes);
  $validation_images = array_slice($train_images, 0, $validation_size - 0);
  $validation_labels = array_slice($train_labels, 0, $validation_size - 0);
  $train_images_rest = array_slice($train_images, $validation_size, count($train_images) - $validation_size);
  $train_labels_rest = array_slice($train_labels, $validation_size, count($train_labels) - $validation_size);
  $train = new_dataset($train_images_rest, $train_labels_rest);
  $validation = new_dataset($validation_images, $validation_labels);
  $testset = new_dataset($test_images, $test_labels);
  return ['train' => $train, 'validation' => $validation, 'test_ds' => $testset];
};
  function main() {
  $train_images = [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5]];
  $train_labels_raw = [0, 1, 2, 3, 4];
  $test_images = [[5, 6], [6, 7]];
  $test_labels_raw = [5, 6];
  $data = read_data_sets($train_images, $train_labels_raw, $test_images, $test_labels_raw, 2, 10);
  $ds = $data['train'];
  $res = next_batch($ds, 2);
  $ds = $res['dataset'];
  echo rtrim(_str($res['images'])), PHP_EOL;
  echo rtrim(_str($res['labels'])), PHP_EOL;
  $res = next_batch($ds, 2);
  $ds = $res['dataset'];
  echo rtrim(_str($res['images'])), PHP_EOL;
  echo rtrim(_str($res['labels'])), PHP_EOL;
  $res = next_batch($ds, 2);
  $ds = $res['dataset'];
  echo rtrim(_str($res['images'])), PHP_EOL;
  echo rtrim(_str($res['labels'])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
