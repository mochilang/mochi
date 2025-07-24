<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
function sinApprox($x) {
  global $PI, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $pad, $meanTime, $main;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function cosApprox($x) {
  global $PI, $sinApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $pad, $meanTime, $main;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n - 1) * (2 * $n)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function atanApprox($x) {
  global $PI, $sinApprox, $cosApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $pad, $meanTime, $main;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
}
function atan2Approx($y, $x) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $digit, $parseTwo, $parseSec, $pad, $meanTime, $main;
  if ($x > 0.0) {
  return atanApprox($y / $x);
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
}
function digit($ch) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $parseTwo, $parseSec, $pad, $meanTime, $main;
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0;
}
function parseTwo($s, $idx) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseSec, $pad, $meanTime, $main;
  return digit(substr($s, $idx, $idx + 1 - $idx)) * 10 + digit(substr($s, $idx + 1, $idx + 2 - $idx + 1));
}
function parseSec($s) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $pad, $meanTime, $main;
  $h = parseTwo($s, 0);
  $m = parseTwo($s, 3);
  $sec = parseTwo($s, 6);
  $tmp = ($h * 60 + $m) * 60 + $sec;
  return floatval($tmp);
}
function pad($n) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $meanTime, $main;
  if ($n < 10) {
  return '0' . json_encode($n, 1344);
}
  return json_encode($n, 1344);
}
function meanTime($times) {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $pad, $main;
  $ssum = 0.0;
  $csum = 0.0;
  $i = 0;
  while ($i < count($times)) {
  $sec = parseSec($times[$i]);
  $ang = $sec * 2.0 * $PI / 86400.0;
  $ssum = $ssum + sinApprox($ang);
  $csum = $csum + cosApprox($ang);
  $i = $i + 1;
};
  $theta = atan2Approx($ssum, $csum);
  $frac = $theta / (2.0 * $PI);
  while ($frac < 0.0) {
  $frac = $frac + 1.0;
};
  $total = $frac * 86400.0;
  $si = intval($total);
  $h = intval((intdiv($si, 3600)));
  $m = intval((intdiv(($si % 3600), 60)));
  $s = intval(($si % 60));
  return pad($h) . ':' . pad($m) . ':' . pad($s);
}
function main() {
  global $PI, $sinApprox, $cosApprox, $atanApprox, $atan2Approx, $digit, $parseTwo, $parseSec, $pad, $meanTime;
  $inputs = ['23:00:17', '23:40:20', '00:12:45', '00:17:19'];
  echo rtrim(meanTime($inputs)), PHP_EOL;
}
main();
