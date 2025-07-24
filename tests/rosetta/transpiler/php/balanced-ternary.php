<?php
ini_set('memory_limit', '-1');
function trimLeftZeros($s) {
  global $btString, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $show, $main;
  $i = 0;
  while ($i < strlen($s) && substr($s, $i, $i + 1 - $i) == '0') {
  $i = $i + 1;
};
  return substr($s, $i, strlen($s) - $i);
}
function btString($s) {
  global $trimLeftZeros, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $show, $main;
  $s = trimLeftZeros($s);
  $b = [];
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '+') {
  $b = array_merge($b, [1]);
} else {
  if ($ch == '0') {
  $b = array_merge($b, [0]);
} else {
  if ($ch == '-') {
  $b = array_merge($b, [0 - 1]);
} else {
  return ['bt' => [], 'ok' => false];
};
};
}
  $i = $i - 1;
};
  return ['bt' => $b, 'ok' => true];
}
function btToString($b) {
  global $trimLeftZeros, $btString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $show, $main;
  if (count($b) == 0) {
  return '0';
}
  $r = '';
  $i = count($b) - 1;
  while ($i >= 0) {
  $d = $b[$i];
  if ($d == 0 - 1) {
  $r = $r . '-';
} else {
  if ($d == 0) {
  $r = $r . '0';
} else {
  $r = $r . '+';
};
}
  $i = $i - 1;
};
  return $r;
}
function btInt($i) {
  global $trimLeftZeros, $btString, $btToString, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $show, $main;
  if ($i == 0) {
  return [];
}
  $n = $i;
  $b = [];
  while ($n != 0) {
  $m = $n % 3;
  $n = intval((intdiv($n, 3)));
  if ($m == 2) {
  $m = 0 - 1;
  $n = $n + 1;
} else {
  if ($m == 0 - 2) {
  $m = 1;
  $n = $n - 1;
};
}
  $b = array_merge($b, [$m]);
};
  return $b;
}
function btToInt($b) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btNeg, $btAdd, $btMul, $padLeft, $show, $main;
  $r = 0;
  $pt = 1;
  $i = 0;
  while ($i < count($b)) {
  $r = $r + $b[$i] * $pt;
  $pt = $pt * 3;
  $i = $i + 1;
};
  return $r;
}
function btNeg($b) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btAdd, $btMul, $padLeft, $show, $main;
  $r = [];
  $i = 0;
  while ($i < count($b)) {
  $r = array_merge($r, [-$b[$i]]);
  $i = $i + 1;
};
  return $r;
}
function btAdd($a, $b) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btNeg, $btMul, $padLeft, $show, $main;
  return btInt(btToInt($a) + btToInt($b));
}
function btMul($a, $b) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $padLeft, $show, $main;
  return btInt(btToInt($a) * btToInt($b));
}
function padLeft($s, $w) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $show, $main;
  $r = $s;
  while (strlen($r) < $w) {
  $r = ' ' . $r;
};
  return $r;
}
function show($label, $b) {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $main;
  $l = padLeft($label, 7);
  $bs = padLeft(btToString($b), 12);
  $is = padLeft(json_encode(btToInt($b), 1344), 7);
  echo rtrim($l . ' ' . $bs . ' ' . $is), PHP_EOL;
}
function main() {
  global $trimLeftZeros, $btString, $btToString, $btInt, $btToInt, $btNeg, $btAdd, $btMul, $padLeft, $show;
  $ares = btString('+-0++0+');
  $a = $ares['bt'];
  $b = btInt(-436);
  $cres = btString('+-++-');
  $c = $cres['bt'];
  show('a:', $a);
  show('b:', $b);
  show('c:', $c);
  show('a(b-c):', btMul($a, btAdd($b, btNeg($c))));
}
main();
