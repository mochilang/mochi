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
  function round_int($x) {
  return intval(($x + 0.5));
};
  function rgb_to_cmyk($r_input, $g_input, $b_input) {
  if ($r_input < 0 || $r_input >= 256 || $g_input < 0 || $g_input >= 256 || $b_input < 0 || $b_input >= 256) {
  $panic('Expected int of the range 0..255');
}
  $r = (floatval($r_input)) / 255.0;
  $g = (floatval($g_input)) / 255.0;
  $b = (floatval($b_input)) / 255.0;
  $max_val = $r;
  if ($g > $max_val) {
  $max_val = $g;
}
  if ($b > $max_val) {
  $max_val = $b;
}
  $k_float = 1.0 - $max_val;
  if ($k_float == 1.0) {
  return [0, 0, 0, 100];
}
  $c_float = 100.0 * (1.0 - $r - $k_float) / (1.0 - $k_float);
  $m_float = 100.0 * (1.0 - $g - $k_float) / (1.0 - $k_float);
  $y_float = 100.0 * (1.0 - $b - $k_float) / (1.0 - $k_float);
  $k_percent = 100.0 * $k_float;
  $c = round_int($c_float);
  $m = round_int($m_float);
  $y = round_int($y_float);
  $k = round_int($k_percent);
  return [$c, $m, $y, $k];
};
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(255, 255, 255), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(128, 128, 128), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(0, 0, 0), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(255, 0, 0), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(0, 255, 0), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rgb_to_cmyk(0, 0, 255), 1344))))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
