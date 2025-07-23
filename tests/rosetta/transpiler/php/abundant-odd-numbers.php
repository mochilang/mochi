<?php
ini_set('memory_limit','-1');
function _append($a, $b) {
    $a[] = $b;
    return $a;
}
function divisors($n) {
  global $sum, $sumStr, $pad2, $pad5, $abundantOdd, $main;
  $divs = [1];
  $divs2 = [];
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  $j = intval((intdiv($n, $i)));
  $divs = _append($divs, $i);
  if ($i != $j) {
  $divs2 = _append($divs2, $j);
};
}
  $i = $i + 1;
};
  $j = count($divs2) - 1;
  while ($j >= 0) {
  $divs = _append($divs, $divs2[$j]);
  $j = $j - 1;
};
  return $divs;
}
function sum($xs) {
  global $divisors, $sumStr, $pad2, $pad5, $abundantOdd, $main;
  $tot = 0;
  foreach ($xs as $v) {
  $tot = $tot + $v;
};
  return $tot;
}
function sumStr($xs) {
  global $divisors, $sum, $pad2, $pad5, $abundantOdd, $main;
  $s = "";
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . json_encode($xs[$i], 1344) . " + ";
  $i = $i + 1;
};
  return substr($s, 0, strlen($s) - 3 - 0);
}
function pad2($n) {
  global $divisors, $sum, $sumStr, $pad5, $abundantOdd, $main;
  $s = json_encode($n, 1344);
  if (strlen($s) < 2) {
  return " " . $s;
}
  return $s;
}
function pad5($n) {
  global $divisors, $sum, $sumStr, $pad2, $abundantOdd, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < 5) {
  $s = " " . $s;
};
  return $s;
}
function abundantOdd($searchFrom, $countFrom, $countTo, $printOne) {
  global $divisors, $sum, $sumStr, $pad2, $pad5, $main;
  $count = $countFrom;
  $n = $searchFrom;
  while ($count < $countTo) {
  $divs = divisors($n);
  $tot = array_sum($divs);
  if ($tot > $n) {
  $count = $count + 1;
  if ($printOne && $count < $countTo) {
  $n = $n + 2;
  continue;
};
  $s = sumStr($divs);
  if (!$printOne) {
  echo pad2($count) . ". " . pad5($n) . " < " . $s . " = " . json_encode($tot, 1344), PHP_EOL;
} else {
  echo json_encode($n, 1344) . " < " . $s . " = " . json_encode($tot, 1344), PHP_EOL;
};
}
  $n = $n + 2;
};
  return $n;
}
function main() {
  global $divisors, $sum, $sumStr, $pad2, $pad5, $abundantOdd;
  $max = 25;
  echo "The first " . json_encode($max, 1344) . " abundant odd numbers are:", PHP_EOL;
  $n = abundantOdd(1, 0, $max, false);
  echo "\nThe one thousandth abundant odd number is:", PHP_EOL;
  abundantOdd($n, $max, 1000, true);
  echo "\nThe first abundant odd number above one billion is:", PHP_EOL;
  abundantOdd(1000000001, 0, 1, true);
}
main();
