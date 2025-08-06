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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function segment_image($image, $thresholds) {
  $segmented = [];
  $i = 0;
  while ($i < count($image)) {
  $row = [];
  $j = 0;
  while ($j < count($image[$i])) {
  $pixel = $image[$i][$j];
  $label = 0;
  $k = 0;
  while ($k < count($thresholds)) {
  if ($pixel > $thresholds[$k]) {
  $label = $k + 1;
}
  $k = $k + 1;
};
  $row = _append($row, $label);
  $j = $j + 1;
};
  $segmented = _append($segmented, $row);
  $i = $i + 1;
};
  return $segmented;
};
  function main() {
  $image = [[80, 120, 180], [40, 90, 150], [20, 60, 100]];
  $thresholds = [50, 100, 150];
  $segmented = segment_image($image, $thresholds);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($segmented, 1344))))))), PHP_EOL;
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
