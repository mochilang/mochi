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
  function flip_horizontal_image($img) {
  global $image, $boxes, $h_img, $h_boxes, $v_img, $v_boxes;
  $flipped = [];
  $i = 0;
  while ($i < count($img)) {
  $row = $img[$i];
  $j = count($row) - 1;
  $new_row = [];
  while ($j >= 0) {
  $new_row = _append($new_row, $row[$j]);
  $j = $j - 1;
};
  $flipped = _append($flipped, $new_row);
  $i = $i + 1;
};
  return $flipped;
};
  function flip_vertical_image($img) {
  global $image, $boxes, $h_img, $h_boxes, $v_img, $v_boxes;
  $flipped = [];
  $i = count($img) - 1;
  while ($i >= 0) {
  $flipped = _append($flipped, $img[$i]);
  $i = $i - 1;
};
  return $flipped;
};
  function flip_horizontal_boxes($boxes) {
  global $image, $h_img, $h_boxes, $v_img, $v_boxes;
  $result = [];
  $i = 0;
  while ($i < count($boxes)) {
  $b = $boxes[$i];
  $x_new = 1.0 - $b[1];
  $result = _append($result, [$b[0], $x_new, $b[2], $b[3], $b[4]]);
  $i = $i + 1;
};
  return $result;
};
  function flip_vertical_boxes($boxes) {
  global $image, $h_img, $h_boxes, $v_img, $v_boxes;
  $result = [];
  $i = 0;
  while ($i < count($boxes)) {
  $b = $boxes[$i];
  $y_new = 1.0 - $b[2];
  $result = _append($result, [$b[0], $b[1], $y_new, $b[3], $b[4]]);
  $i = $i + 1;
};
  return $result;
};
  function print_image($img) {
  global $image, $boxes, $h_img, $h_boxes, $v_img, $v_boxes;
  $i = 0;
  while ($i < count($img)) {
  $row = $img[$i];
  $j = 0;
  $line = '';
  while ($j < count($row)) {
  $line = $line . _str($row[$j]) . ' ';
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  $image = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
  $boxes = [[0.0, 0.25, 0.25, 0.5, 0.5], [1.0, 0.75, 0.75, 0.5, 0.5]];
  echo rtrim('Original image:'), PHP_EOL;
  print_image($image);
  echo rtrim(_str($boxes)), PHP_EOL;
  echo rtrim('Horizontal flip:'), PHP_EOL;
  $h_img = flip_horizontal_image($image);
  $h_boxes = flip_horizontal_boxes($boxes);
  print_image($h_img);
  echo rtrim(_str($h_boxes)), PHP_EOL;
  echo rtrim('Vertical flip:'), PHP_EOL;
  $v_img = flip_vertical_image($image);
  $v_boxes = flip_vertical_boxes($boxes);
  print_image($v_img);
  echo rtrim(_str($v_boxes)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
