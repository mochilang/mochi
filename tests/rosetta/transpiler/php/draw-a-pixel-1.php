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
$__start_mem = memory_get_usage();
$__start = _now();
  $width = 320;
  $height = 240;
  $img = [];
  $y = 0;
  while ($y < $height) {
  $row = [];
  $x = 0;
  while ($x < $width) {
  $row = array_merge($row, ['green']);
  $x = $x + 1;
};
  $img = array_merge($img, [$row]);
  $y = $y + 1;
}
  $img[100][100] = 'red';
  echo rtrim('The color of the pixel at (  0,   0) is ' . $img[0][0] . '.'), PHP_EOL;
  echo rtrim('The color of the pixel at (100, 100) is ' . $img[100][100] . '.'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
