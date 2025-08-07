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
  function hamming($n) {
  if ($n < 1) {
  $panic('n_element should be a positive number');
}
  $hamming_list = [1];
  $i = 0;
  $j = 0;
  $k = 0;
  $index = 1;
  while ($index < $n) {
  while ($hamming_list[$i] * 2 <= $hamming_list[count($hamming_list) - 1]) {
  $i = $i + 1;
};
  while ($hamming_list[$j] * 3 <= $hamming_list[count($hamming_list) - 1]) {
  $j = $j + 1;
};
  while ($hamming_list[$k] * 5 <= $hamming_list[count($hamming_list) - 1]) {
  $k = $k + 1;
};
  $m1 = $hamming_list[$i] * 2;
  $m2 = $hamming_list[$j] * 3;
  $m3 = $hamming_list[$k] * 5;
  $next = $m1;
  if ($m2 < $next) {
  $next = $m2;
}
  if ($m3 < $next) {
  $next = $m3;
}
  $hamming_list = _append($hamming_list, $next);
  $index = $index + 1;
};
  return $hamming_list;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(hamming(5), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(hamming(10), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(hamming(15), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
