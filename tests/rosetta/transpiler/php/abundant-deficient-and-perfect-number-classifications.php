<?php
ini_set('memory_limit','-1');
function pfacSum($i) {
  global $main;
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
function main() {
  global $pfacSum;
  $d = 0;
  $a = 0;
  $pnum = 0;
  $i = 1;
  while ($i <= 20000) {
  $j = pfacSum($i);
  if ($j < $i) {
  $d = $d + 1;
}
  if ($j == $i) {
  $pnum = $pnum + 1;
}
  if ($j > $i) {
  $a = $a + 1;
}
  $i = $i + 1;
};
  echo "There are " . json_encode($d, 1344) . " deficient numbers between 1 and 20000", PHP_EOL;
  echo "There are " . json_encode($a, 1344) . " abundant numbers  between 1 and 20000", PHP_EOL;
  echo "There are " . json_encode($pnum, 1344) . " perfect numbers between 1 and 20000", PHP_EOL;
}
main();
