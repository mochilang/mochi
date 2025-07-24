<?php
ini_set('memory_limit', '-1');
function powf($base, $exp) {
  global $nthRoot, $main;
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function nthRoot($x, $n) {
  global $powf, $main;
  $low = 0.0;
  $high = $x;
  $i = 0;
  while ($i < 60) {
  $mid = ($low + $high) / 2.0;
  if (powf($mid, $n) > $x) {
  $high = $mid;
} else {
  $low = $mid;
}
  $i = $i + 1;
};
  return $low;
}
function main() {
  global $powf, $nthRoot;
  $sum = 0.0;
  $sumRecip = 0.0;
  $prod = 1.0;
  $n = 1;
  while ($n <= 10) {
  $f = floatval($n);
  $sum = $sum + $f;
  $sumRecip = $sumRecip + 1.0 / $f;
  $prod = $prod * $f;
  $n = $n + 1;
};
  $count = 10.0;
  $a = $sum / $count;
  $g = nthRoot($prod, 10);
  $h = $count / $sumRecip;
  echo rtrim('A: ' . json_encode($a, 1344) . ' G: ' . json_encode($g, 1344) . ' H: ' . json_encode($h, 1344)), PHP_EOL;
  echo rtrim('A >= G >= H: ' . json_encode($a >= $g && $g >= $h, 1344)), PHP_EOL;
}
main();
