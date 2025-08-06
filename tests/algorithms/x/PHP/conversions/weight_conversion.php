<?php
ini_set('memory_limit', '-1');
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
}
$KILOGRAM_CHART = ['kilogram' => 1.0, 'gram' => 1000.0, 'milligram' => 1000000.0, 'metric-ton' => 0.001, 'long-ton' => 0.0009842073, 'short-ton' => 0.0011023122, 'pound' => 2.2046244202, 'stone' => 0.1574731728, 'ounce' => 35.273990723, 'carrat' => 5000.0, 'atomic-mass-unit' => 6.022136652 * pow10(26)];
$WEIGHT_TYPE_CHART = ['kilogram' => 1.0, 'gram' => 0.001, 'milligram' => 0.000001, 'metric-ton' => 1000.0, 'long-ton' => 1016.04608, 'short-ton' => 907.184, 'pound' => 0.453592, 'stone' => 6.35029, 'ounce' => 0.0283495, 'carrat' => 0.0002, 'atomic-mass-unit' => 1.660540199 * pow10(-27)];
function weight_conversion($from_type, $to_type, $value) {
  global $KILOGRAM_CHART, $WEIGHT_TYPE_CHART;
  $has_to = array_key_exists($to_type, $KILOGRAM_CHART);
  $has_from = array_key_exists($from_type, $WEIGHT_TYPE_CHART);
  if ($has_to && $has_from) {
  return $value * $KILOGRAM_CHART[$to_type] * $WEIGHT_TYPE_CHART[$from_type];
}
  echo rtrim('Invalid \'from_type\' or \'to_type\''), PHP_EOL;
  return 0.0;
}
echo rtrim(json_encode(weight_conversion('kilogram', 'gram', 1.0), 1344)), PHP_EOL;
echo rtrim(json_encode(weight_conversion('gram', 'pound', 3.0), 1344)), PHP_EOL;
echo rtrim(json_encode(weight_conversion('ounce', 'kilogram', 3.0), 1344)), PHP_EOL;
