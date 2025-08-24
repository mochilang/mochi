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
$__start_mem = memory_get_usage();
$__start = _now();
  function pow10($exp) {
  global $KILOGRAM_CHART, $WEIGHT_TYPE_CHART;
  $result = 1.0;
  if ($exp >= 0) {
  $i = 0;
  while ($i < $exp) {
  $result = $result * 10.0;
  $i = $i + 1;
};
} else {
  $i = 0;
  while ($i < (0 - $exp)) {
  $result = $result / 10.0;
  $i = $i + 1;
};
}
  return $result;
};
  $KILOGRAM_CHART = ['atomic-mass-unit' => 602213665200000036747951526.19481086730957, 'carrat' => 5000.0, 'gram' => 1000.0, 'kilogram' => 1.0, 'long-ton' => 0.0009842073, 'metric-ton' => 0.001, 'milligram' => 1000000.0, 'ounce' => 35.273990723, 'pound' => 2.2046244202, 'short-ton' => 0.0011023122, 'stone' => 0.1574731728];
  $WEIGHT_TYPE_CHART = ['atomic-mass-unit' => 1.660540199 * pow10(-27), 'carrat' => 0.0002, 'gram' => 0.001, 'kilogram' => 1.0, 'long-ton' => 1016.04608, 'metric-ton' => 1000.0, 'milligram' => 0.000001, 'ounce' => 0.0283495, 'pound' => 0.453592, 'short-ton' => 907.184, 'stone' => 6.35029];
  function weight_conversion($from_type, $to_type, $value) {
  global $KILOGRAM_CHART, $WEIGHT_TYPE_CHART;
  $has_to = array_key_exists($to_type, $KILOGRAM_CHART);
  $has_from = array_key_exists($from_type, $WEIGHT_TYPE_CHART);
  if ($has_to && $has_from) {
  return $value * $KILOGRAM_CHART[$to_type] * $WEIGHT_TYPE_CHART[$from_type];
}
  echo rtrim('Invalid \'from_type\' or \'to_type\''), PHP_EOL;
  return 0.0;
};
  echo rtrim(json_encode(weight_conversion('kilogram', 'gram', 1.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(weight_conversion('gram', 'pound', 3.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(weight_conversion('ounce', 'kilogram', 3.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
