<?php
ini_set('memory_limit', '-1');
function mochi_floor($x) {
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
}
function pow10($n) {
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
}
function round_to($x, $ndigits) {
  $m = pow10($ndigits);
  return mochi_floor($x * $m + 0.5) / $m;
}
function celsius_to_fahrenheit($c, $ndigits) {
  return round_to($c * 9.0 / 5.0 + 32.0, $ndigits);
}
function celsius_to_kelvin($c, $ndigits) {
  return round_to($c + 273.15, $ndigits);
}
function celsius_to_rankine($c, $ndigits) {
  return round_to($c * 9.0 / 5.0 + 491.67, $ndigits);
}
function fahrenheit_to_celsius($f, $ndigits) {
  return round_to(($f - 32.0) * 5.0 / 9.0, $ndigits);
}
function fahrenheit_to_kelvin($f, $ndigits) {
  return round_to(($f - 32.0) * 5.0 / 9.0 + 273.15, $ndigits);
}
function fahrenheit_to_rankine($f, $ndigits) {
  return round_to($f + 459.67, $ndigits);
}
function kelvin_to_celsius($k, $ndigits) {
  return round_to($k - 273.15, $ndigits);
}
function kelvin_to_fahrenheit($k, $ndigits) {
  return round_to(($k - 273.15) * 9.0 / 5.0 + 32.0, $ndigits);
}
function kelvin_to_rankine($k, $ndigits) {
  return round_to($k * 9.0 / 5.0, $ndigits);
}
function rankine_to_celsius($r, $ndigits) {
  return round_to(($r - 491.67) * 5.0 / 9.0, $ndigits);
}
function rankine_to_fahrenheit($r, $ndigits) {
  return round_to($r - 459.67, $ndigits);
}
function rankine_to_kelvin($r, $ndigits) {
  return round_to($r * 5.0 / 9.0, $ndigits);
}
function reaumur_to_kelvin($r, $ndigits) {
  return round_to($r * 1.25 + 273.15, $ndigits);
}
function reaumur_to_fahrenheit($r, $ndigits) {
  return round_to($r * 2.25 + 32.0, $ndigits);
}
function reaumur_to_celsius($r, $ndigits) {
  return round_to($r * 1.25, $ndigits);
}
function reaumur_to_rankine($r, $ndigits) {
  return round_to($r * 2.25 + 32.0 + 459.67, $ndigits);
}
echo rtrim(json_encode(celsius_to_fahrenheit(0.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(celsius_to_kelvin(0.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(celsius_to_rankine(0.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(fahrenheit_to_celsius(32.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(fahrenheit_to_kelvin(32.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(fahrenheit_to_rankine(32.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(kelvin_to_celsius(273.15, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(kelvin_to_fahrenheit(273.15, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(kelvin_to_rankine(273.15, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(rankine_to_celsius(491.67, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(rankine_to_fahrenheit(491.67, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(rankine_to_kelvin(491.67, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(reaumur_to_kelvin(80.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(reaumur_to_fahrenheit(80.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(reaumur_to_celsius(80.0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(reaumur_to_rankine(80.0, 2), 1344)), PHP_EOL;
