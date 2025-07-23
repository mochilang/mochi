<?php
function pow_big($base, $exp) {
  global $bit_len, $err, $ackermann2, $show, $main;
  $result = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = $result * $b;
}
  $b = $b * $b;
  $e = intval((intdiv($e, 2)));
};
  return $result;
}
function bit_len($x) {
  global $pow_big, $err, $ackermann2, $show, $main;
  $n = $x;
  $c = 0;
  while ($n > 0) {
  $n = $n / 2;
  $c = $c + 1;
};
  return $c;
}
$err = "";
function ackermann2($m, $n) {
  global $pow_big, $bit_len, $err, $show, $main;
  if ($err != "") {
  return 0;
}
  if ($m <= 3) {
  $mi = intval($m);
  if ($mi == 0) {
  return $n + 1;
};
  if ($mi == 1) {
  return $n + 2;
};
  if ($mi == 2) {
  return 2 * $n + 3;
};
  if ($mi == 3) {
  $nb = bit_len($n);
  if ($nb > 64) {
  $err = "A(m,n) had n of " . json_encode($nb, 1344) . " bits; too large";
  return 0;
};
  $r = pow_big(2, intval($n));
  return 8 * $r - 3;
};
}
  if (bit_len($n) == 0) {
  return ackermann2($m - (1), 1);
}
  return ackermann2($m - (1), ackermann2($m, $n - (1)));
}
function show($m, $n) {
  global $pow_big, $bit_len, $err, $ackermann2, $main;
  $err = "";
  $res = ackermann2($m, $n);
  if ($err != "") {
  echo "A(" . json_encode($m, 1344) . ", " . json_encode($n, 1344) . ") = Error: " . $err, PHP_EOL;
  return;
}
  if (bit_len($res) <= 256) {
  echo "A(" . json_encode($m, 1344) . ", " . json_encode($n, 1344) . ") = " . json_encode($res, 1344), PHP_EOL;
} else {
  $s = json_encode($res, 1344);
  $pre = substr($s, 0, 20 - 0);
  $suf = substr($s, strlen($s) - 20, strlen($s) - strlen($s) - 20);
  echo "A(" . json_encode($m, 1344) . ", " . json_encode($n, 1344) . ") = " . json_encode(strlen($s), 1344) . " digits starting/ending with: " . $pre . "..." . $suf, PHP_EOL;
}
}
function main() {
  global $pow_big, $bit_len, $err, $ackermann2, $show;
  show(0, 0);
  show(1, 2);
  show(2, 4);
  show(3, 100);
  show(3, 1000000);
  show(4, 1);
  show(4, 2);
  show(4, 3);
}
main();
