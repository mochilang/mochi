<?php
function intSqrt($x) {
  global $sumRecip, $main;
  if ($x < 2) {
  return $x;
}
  $left = 1;
  $right = intdiv($x, 2);
  $ans = 0;
  while ($left <= $right) {
  $mid = $left + ($right - $left) / 2;
  $sq = $mid * $mid;
  if ($sq == $x) {
  return $mid;
}
  if ($sq < $x) {
  $left = $mid + 1;
  $ans = $mid;
} else {
  $right = $mid - 1;
}
};
  return $ans;
}
function sumRecip($n) {
  global $intSqrt, $main;
  $s = 1;
  $limit = intSqrt($n);
  $f = 2;
  while ($f <= $limit) {
  if ($n % $f == 0) {
  $s = $s + intdiv($n, $f);
  $f2 = intdiv($n, $f);
  if ($f2 != $f) {
  $s = $s + $f;
};
}
  $f = $f + 1;
};
  return $s;
}
function main() {
  global $intSqrt, $sumRecip;
  $nums = [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776];
  foreach ($nums as $n) {
  $s = sumRecip($n);
  if ($s % $n == 0) {
  $val = intdiv($s, $n);
  $perfect = "";
  if ($val == 1) {
  $perfect = "perfect!";
};
  echo "Sum of recipr. factors of " . json_encode($n, 1344) . " = " . json_encode($val, 1344) . " exactly " . $perfect, PHP_EOL;
}
};
}
main();
