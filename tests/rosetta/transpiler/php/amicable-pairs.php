<?php
function pfacSum($i) {
  global $pad, $main;
  $sum = 0;
  $p = 1;
  while ($p <= intdiv($i, 2)) {
  if ($i % $p == 0) {
  $sum = $sum + $p;
}
  $p = $p + 1;
};
  return $sum;
}
function pad($n, $width) {
  global $pfacSum, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < $width) {
  $s = " " . $s;
};
  return $s;
}
function main() {
  global $pfacSum, $pad;
  $sums = [];
  $i = 0;
  while ($i < 20000) {
  $sums = array_merge($sums, [0]);
  $i = $i + 1;
};
  $i = 1;
  while ($i < 20000) {
  $sums[$i] = pfacSum($i);
  $i = $i + 1;
};
  echo "The amicable pairs below 20,000 are:", PHP_EOL;
  $n = 2;
  while ($n < 19999) {
  $m = $sums[$n];
  if ($m > $n && $m < 20000 && $n == $sums[$m]) {
  echo "  " . pad($n, 5) . " and " . pad($m, 5), PHP_EOL;
}
  $n = $n + 1;
};
}
main();
