<?php
function primeFactors($n) {
  global $repeat, $D, $pad, $main;
  $factors = [];
  $x = $n;
  while ($x % 2 == 0) {
  $factors = array_merge($factors, [2]);
  $x = intval((intdiv($x, 2)));
};
  $p = 3;
  while ($p * $p <= $x) {
  while ($x % $p == 0) {
  $factors = array_merge($factors, [$p]);
  $x = intval((intdiv($x, $p)));
};
  $p = $p + 2;
};
  if ($x > 1) {
  $factors = array_merge($factors, [$x]);
}
  return $factors;
}
function repeat($ch, $n) {
  global $primeFactors, $D, $pad, $main;
  $s = "";
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
}
function D($n) {
  global $primeFactors, $repeat, $pad, $main;
  if ($n < 0.0) {
  return -D(-$n);
}
  if ($n < 2.0) {
  return 0.0;
}
  $factors = [];
  if ($n < 10000000000000000000.0) {
  $factors = primeFactors(intval(($n)));
} else {
  $g = intval(($n / 100.0));
  $factors = primeFactors($g);
  $factors = array_merge($factors, [2]);
  $factors = array_merge($factors, [2]);
  $factors = array_merge($factors, [5]);
  $factors = array_merge($factors, [5]);
}
  $c = count($factors);
  if ($c == 1) {
  return 1.0;
}
  if ($c == 2) {
  return ($factors[0] + $factors[1]);
}
  $d = $n / ($factors[0]);
  return D($d) * ($factors[0]) + $d;
}
function pad($n) {
  global $primeFactors, $repeat, $D, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < 4) {
  $s = " " . $s;
};
  return $s;
}
function main() {
  global $primeFactors, $repeat, $D, $pad;
  $vals = [];
  $n = -99;
  while ($n < 101) {
  $vals = array_merge($vals, [intval((D($n)))]);
  $n = $n + 1;
};
  $i = 0;
  while ($i < count($vals)) {
  $line = "";
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($vals[$i + $j]);
  if ($j < 9) {
  $line = $line . " ";
}
  $j = $j + 1;
};
  echo $line, PHP_EOL;
  $i = $i + 10;
};
  $pow = 1.0;
  $m = 1;
  while ($m < 21) {
  $pow = $pow * 10.0;
  $exp = json_encode($m, 1344);
  if (strlen($exp) < 2) {
  $exp = $exp . " ";
}
  $res = json_encode($m, 1344) . repeat("0", $m - 1);
  echo "D(10^" . $exp . ") / 7 = " . $res, PHP_EOL;
  $m = $m + 1;
};
}
main();
