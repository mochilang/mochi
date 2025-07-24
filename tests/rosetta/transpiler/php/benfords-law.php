<?php
ini_set('memory_limit', '-1');
function floorf($x) {
  global $indexOf, $fmtF3, $padFloat3, $fib1000, $leadingDigit, $show, $main;
  $y = intval($x);
  return floatval($y);
}
function indexOf($s, $ch) {
  global $floorf, $fmtF3, $padFloat3, $fib1000, $leadingDigit, $show, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fmtF3($x) {
  global $floorf, $indexOf, $padFloat3, $fib1000, $leadingDigit, $show, $main;
  $y = floorf($x * 1000.0 + 0.5) / 1000.0;
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
function padFloat3($x, $width) {
  global $floorf, $indexOf, $fmtF3, $fib1000, $leadingDigit, $show, $main;
  $s = fmtF3($x);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function fib1000() {
  global $floorf, $indexOf, $fmtF3, $padFloat3, $leadingDigit, $show, $main;
  $a = 0.0;
  $b = 1.0;
  $res = [];
  $i = 0;
  while ($i < 1000) {
  $res = array_merge($res, [$b]);
  $t = $b;
  $b = $b + $a;
  $a = $t;
  $i = $i + 1;
};
  return $res;
}
function leadingDigit($x) {
  global $floorf, $indexOf, $fmtF3, $padFloat3, $fib1000, $show, $main;
  if ($x < 0.0) {
  $x = -$x;
}
  while ($x >= 10.0) {
  $x = $x / 10.0;
};
  while ($x > 0.0 && $x < 1.0) {
  $x = $x * 10.0;
};
  return intval($x);
}
function show($nums, $title) {
  global $floorf, $indexOf, $fmtF3, $padFloat3, $fib1000, $leadingDigit, $main;
  $counts = [0, 0, 0, 0, 0, 0, 0, 0, 0];
  foreach ($nums as $n) {
  $d = leadingDigit($n);
  if ($d >= 1 && $d <= 9) {
  $counts[$d - 1] = $counts[$d - 1] + 1;
}
};
  $preds = [0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046];
  $total = count($nums);
  echo rtrim($title), PHP_EOL;
  echo rtrim('Digit  Observed  Predicted'), PHP_EOL;
  $i = 0;
  while ($i < 9) {
  $obs = (floatval($counts[$i])) / (floatval($total));
  $line = '  ' . json_encode($i + 1, 1344) . '  ' . padFloat3($obs, 9) . '  ' . padFloat3($preds[$i], 8);
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
function main() {
  global $floorf, $indexOf, $fmtF3, $padFloat3, $fib1000, $leadingDigit, $show;
  show(fib1000(), 'First 1000 Fibonacci numbers');
}
main();
