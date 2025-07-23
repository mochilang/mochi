<?php
ini_set('memory_limit','-1');
function _append($a, $b) {
    $a[] = $b;
    return $a;
}
function bigTrim($a) {
  global $bigFromInt, $bigAdd, $bigSub, $bigToString, $minInt, $cumu, $row, $x, $r, $line, $i, $r;
  $n = count($a);
  while ($n > 1 && $a[$n - 1] == 0) {
  $a = array_slice($a, 0, $n - 1 - 0);
  $n = $n - 1;
};
  return $a;
}
function bigFromInt($x) {
  global $bigTrim, $bigAdd, $bigSub, $bigToString, $minInt, $cumu, $row, $r, $line, $i, $r;
  if ($x == 0) {
  return [0];
}
  $digits = [];
  $n = $x;
  while ($n > 0) {
  $digits = _append($digits, $n % 10);
  $n = intdiv($n, 10);
};
  return $digits;
}
function bigAdd($a, $b) {
  global $bigTrim, $bigFromInt, $bigSub, $bigToString, $minInt, $cumu, $row, $x, $r, $line, $r;
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a) || $i < count($b) || $carry > 0) {
  $av = 0;
  if ($i < count($a)) {
  $av = $a[$i];
}
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $s = $av + $bv + $carry;
  $res = _append($res, $s % 10);
  $carry = intdiv($s, 10);
  $i = $i + 1;
};
  return bigTrim($res);
}
function bigSub($a, $b) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigToString, $minInt, $cumu, $row, $x, $r, $line, $r;
  $res = [];
  $borrow = 0;
  $i = 0;
  while ($i < count($a)) {
  $av = $a[$i];
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $diff = $av - $bv - $borrow;
  if ($diff < 0) {
  $diff = $diff + 10;
  $borrow = 1;
} else {
  $borrow = 0;
}
  $res = _append($res, $diff);
  $i = $i + 1;
};
  return bigTrim($res);
}
function bigToString($a) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigSub, $minInt, $cumu, $row, $x, $r, $line, $r;
  $s = "";
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . json_encode($a[$i], 1344);
  $i = $i - 1;
};
  return $s;
}
function minInt($a, $b) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigSub, $bigToString, $cumu, $row, $x, $r, $line, $i, $r;
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
}
function cumu($n) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigSub, $bigToString, $minInt, $r, $line, $i, $r;
  $cache = [[bigFromInt(1)]];
  $y = 1;
  while ($y <= $n) {
  $row = [bigFromInt(0)];
  $x = 1;
  while ($x <= $y) {
  $val = $cache[$y - $x][minInt($x, $y - $x)];
  $row = _append($row, bigAdd($row[count($row) - 1], $val));
  $x = $x + 1;
};
  $cache = _append($cache, $row);
  $y = $y + 1;
};
  return $cache[$n];
}
function row($n) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigSub, $bigToString, $minInt, $cumu, $x, $r, $line, $r;
  $e = cumu($n);
  $out = [];
  $i = 0;
  while ($i < $n) {
  $diff = bigSub($e[$i + 1], $e[$i]);
  $out = _append($out, bigToString($diff));
  $i = $i + 1;
};
  return $out;
}
echo "rows:", PHP_EOL;
$x = 1;
while ($x < 11) {
  $r = row($x);
  $line = "";
  $i = 0;
  while ($i < count($r)) {
  $line = $line . " " . $r[$i] . " ";
  $i = $i + 1;
};
  echo $line, PHP_EOL;
  $x = $x + 1;
}
echo "", PHP_EOL;
echo "sums:", PHP_EOL;
foreach ([23, 123, 1234] as $num) {
  $r = cumu($num);
  echo json_encode($num, 1344) . " " . bigToString($r[count($r) - 1]), PHP_EOL;
}
