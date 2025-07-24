<?php
ini_set('memory_limit', '-1');
function indexOf($s, $ch) {
  global $fmt3, $pad, $smaSeries, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fmt3($x) {
  global $indexOf, $pad, $smaSeries, $main;
  $y = floatval(intval((($x * 1000.0) + 0.5))) / 1000.0;
  $s = json_encode($y, 1344);
  $dot = indexOf($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 3) {
  $s = substr($s, 0, $dot + 4 - 0);
} else {
  while ($decs < 3) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
}
function pad($s, $width) {
  global $indexOf, $fmt3, $smaSeries, $main;
  $out = $s;
  while (strlen($out) < $width) {
  $out = ' ' . $out;
};
  return $out;
}
function smaSeries($xs, $period) {
  global $indexOf, $fmt3, $pad, $main;
  $res = [];
  $sum = 0.0;
  $i = 0;
  while ($i < count($xs)) {
  $sum = $sum + $xs[$i];
  if ($i >= $period) {
  $sum = $sum - $xs[$i - $period];
}
  $denom = $i + 1;
  if ($denom > $period) {
  $denom = $period;
}
  $res = array_merge($res, [$sum / (floatval($denom))]);
  $i = $i + 1;
};
  return $res;
}
function main() {
  global $indexOf, $fmt3, $pad, $smaSeries;
  $xs = [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0];
  $sma3 = smaSeries($xs, 3);
  $sma5 = smaSeries($xs, 5);
  echo rtrim('x       sma3   sma5'), PHP_EOL;
  $i = 0;
  while ($i < count($xs)) {
  $line = pad(fmt3($xs[$i]), 5) . '  ' . pad(fmt3($sma3[$i]), 5) . '  ' . pad(fmt3($sma5[$i]), 5);
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
main();
