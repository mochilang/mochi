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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function clamp($value) {
  global $sample;
  if ($value < 0) {
  return 0;
}
  if ($value > 255) {
  return 255;
}
  return $value;
};
  function change_brightness($img, $level) {
  global $sample;
  if ($level < (-255) || $level > 255) {
  _panic('level must be between -255 and 255');
}
  $result = [];
  $i = 0;
  while ($i < count($img)) {
  $row_res = [];
  $j = 0;
  while ($j < count($img[$i])) {
  $row_res = _append($row_res, clamp($img[$i][$j] + $level));
  $j = $j + 1;
};
  $result = _append($result, $row_res);
  $i = $i + 1;
};
  return $result;
};
  $sample = [[100, 150], [200, 250]];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(change_brightness($sample, 30), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(change_brightness($sample, -60), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
