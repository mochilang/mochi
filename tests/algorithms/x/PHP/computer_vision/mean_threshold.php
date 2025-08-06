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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mean_threshold(&$image) {
  global $img, $result;
  $height = count($image);
  $width = count($image[0]);
  $total = 0;
  $i = 0;
  while ($i < $height) {
  $j = 0;
  while ($j < $width) {
  $total = $total + $image[$i][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  $mean = _intdiv($total, ($height * $width));
  $i = 0;
  while ($i < $height) {
  $j = 0;
  while ($j < $width) {
  if ($image[$i][$j] > $mean) {
  $image[$i][$j] = 255;
} else {
  $image[$i][$j] = 0;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $image;
};
  function print_image($image) {
  global $img, $result;
  $i = 0;
  while ($i < count($image)) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($image[$i], 1344))))))), PHP_EOL;
  $i = $i + 1;
};
};
  $img = [[10, 200, 50], [100, 150, 30], [90, 80, 220]];
  $result = mean_threshold($img);
  print_image($result);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
