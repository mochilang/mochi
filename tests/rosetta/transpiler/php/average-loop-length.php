<?php
ini_set('memory_limit', '-1');
function absf($x) {
  global $floorf, $indexOf, $fmtF, $padInt, $padFloat, $avgLen, $ana, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function floorf($x) {
  global $absf, $indexOf, $fmtF, $padInt, $padFloat, $avgLen, $ana, $main;
  $y = intval($x);
  return floatval($y);
}
function indexOf($s, $ch) {
  global $absf, $floorf, $fmtF, $padInt, $padFloat, $avgLen, $ana, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fmtF($x) {
  global $absf, $floorf, $indexOf, $padInt, $padFloat, $avgLen, $ana, $main;
  $y = floorf($x * 10000.0 + 0.5) / 10000.0;
  $s = json_encode($y, 1344);
  $dot = indexOf($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.0000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 4) {
  $s = substr($s, 0, $dot + 5 - 0);
} else {
  while ($decs < 4) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
}
function padInt($n, $width) {
  global $absf, $floorf, $indexOf, $fmtF, $padFloat, $avgLen, $ana, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function padFloat($x, $width) {
  global $absf, $floorf, $indexOf, $fmtF, $padInt, $avgLen, $ana, $main;
  $s = fmtF($x);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function avgLen($n) {
  global $absf, $floorf, $indexOf, $fmtF, $padInt, $padFloat, $ana, $main;
  $tests = 10000;
  $sum = 0;
  $seed = 1;
  $t = 0;
  while ($t < $tests) {
  $visited = [];
  $i = 0;
  while ($i < $n) {
  $visited = array_merge($visited, [false]);
  $i = $i + 1;
};
  $x = 0;
  while (!$visited[$x]) {
  $visited[$x] = true;
  $sum = $sum + 1;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  $x = $seed % $n;
};
  $t = $t + 1;
};
  return (floatval($sum)) / $tests;
}
function ana($n) {
  global $absf, $floorf, $indexOf, $fmtF, $padInt, $padFloat, $avgLen, $main;
  $nn = floatval($n);
  $term = 1.0;
  $sum = 1.0;
  $i = $nn - 1.0;
  while ($i >= 1.0) {
  $term = $term * ($i / $nn);
  $sum = $sum + $term;
  $i = $i - 1.0;
};
  return $sum;
}
function main() {
  global $absf, $floorf, $indexOf, $fmtF, $padInt, $padFloat, $avgLen, $ana;
  $nmax = 20;
  echo rtrim(' N    average    analytical    (error)'), PHP_EOL;
  echo rtrim('===  =========  ============  ========='), PHP_EOL;
  $n = 1;
  while ($n <= $nmax) {
  $a = avgLen($n);
  $b = ana($n);
  $err = absf($a - $b) / $b * 100.0;
  $line = padInt($n, 3) . '  ' . padFloat($a, 9) . '  ' . padFloat($b, 12) . '  (' . padFloat($err, 6) . '%)';
  echo rtrim($line), PHP_EOL;
  $n = $n + 1;
};
}
main();
