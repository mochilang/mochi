<?php
$THRESHOLD = 140737488355328;
function indexOf($xs, $value) {
  global $THRESHOLD, $contains, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $joinWithCommas, $main;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
}
function contains($xs, $value) {
  global $THRESHOLD, $indexOf, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $joinWithCommas, $main;
  return indexOf($xs, $value) != 0 - 1;
}
function maxOf($a, $b) {
  global $THRESHOLD, $indexOf, $contains, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $joinWithCommas, $main;
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
}
function intSqrt($n) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $joinWithCommas, $main;
  if ($n == 0) {
  return 0;
}
  $x = $n;
  $y = intdiv(($x + 1), 2);
  while ($y < $x) {
  $x = $y;
  $y = intdiv(($x + intdiv($n, $x)), 2);
};
  return $x;
}
function sumProperDivisors($n) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $classifySequence, $padLeft, $padRight, $joinWithCommas, $main;
  if ($n < 2) {
  return 0;
}
  $sqrt = intSqrt($n);
  $sum = 1;
  $i = 2;
  while ($i <= $sqrt) {
  if ($n % $i == 0) {
  $sum = $sum + $i + intdiv($n, $i);
}
  $i = $i + 1;
};
  if ($sqrt * $sqrt == $n) {
  $sum = $sum - $sqrt;
}
  return $sum;
}
function classifySequence($k) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $sumProperDivisors, $padLeft, $padRight, $joinWithCommas, $main;
  $last = $k;
  $seq = [$k];
  while (true) {
  $last = sumProperDivisors($last);
  $seq = array_merge($seq, [$last]);
  $n = count($seq);
  $aliquot = '';
  if ($last == 0) {
  $aliquot = 'Terminating';
} else {
  if ($n == 2 && $last == $k) {
  $aliquot = 'Perfect';
} else {
  if ($n == 3 && $last == $k) {
  $aliquot = 'Amicable';
} else {
  if ($n >= 4 && $last == $k) {
  $aliquot = 'Sociable[' . json_encode($n - 1, 1344) . ']';
} else {
  if ($last == $seq[$n - 2]) {
  $aliquot = 'Aspiring';
} else {
  if (contains(array_slice($seq, 1, maxOf(1, $n - 2) - 1), $last)) {
  $idx = indexOf($seq, $last);
  $aliquot = 'Cyclic[' . json_encode($n - 1 - $idx, 1344) . ']';
} else {
  if ($n == 16 || $last > $THRESHOLD) {
  $aliquot = 'Non-Terminating';
};
};
};
};
};
};
}
  if ($aliquot != '') {
  return ['seq' => $seq, 'aliquot' => $aliquot];
}
};
  return ['seq' => $seq, 'aliquot' => ''];
}
function padLeft($n, $w) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padRight, $joinWithCommas, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < $w) {
  $s = ' ' . $s;
};
  return $s;
}
function padRight($s, $w) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $joinWithCommas, $main;
  $r = $s;
  while (strlen($r) < $w) {
  $r = $r . ' ';
};
  return $r;
}
function joinWithCommas($seq) {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $main;
  $s = '[';
  $i = 0;
  while ($i < count($seq)) {
  $s = $s . json_encode($seq[$i], 1344);
  if ($i < count($seq) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function main() {
  global $THRESHOLD, $indexOf, $contains, $maxOf, $intSqrt, $sumProperDivisors, $classifySequence, $padLeft, $padRight, $joinWithCommas;
  echo 'Aliquot classifications - periods for Sociable/Cyclic in square brackets:
', PHP_EOL;
  $k = 1;
  while ($k <= 10) {
  $res = classifySequence($k);
  echo padLeft($k, 2) . ': ' . padRight(strval($res['aliquot']), 15) . ' ' . joinWithCommas($res['seq']), PHP_EOL;
  $k = $k + 1;
};
  echo '', PHP_EOL;
  $s = [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488];
  $i = 0;
  while ($i < count($s)) {
  $val = $s[$i];
  $res = classifySequence($val);
  echo padLeft($val, 7) . ': ' . padRight(strval($res['aliquot']), 15) . ' ' . joinWithCommas($res['seq']), PHP_EOL;
  $i = $i + 1;
};
  echo '', PHP_EOL;
  $big = 15355717786080;
  $r = classifySequence($big);
  echo json_encode($big, 1344) . ': ' . padRight(strval($r['aliquot']), 15) . ' ' . joinWithCommas($r['seq']), PHP_EOL;
}
main();
