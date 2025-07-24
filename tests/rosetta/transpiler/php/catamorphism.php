<?php
ini_set('memory_limit', '-1');
function add($a, $b) {
  global $sub, $mul, $fold, $n;
  return $a + $b;
}
function sub($a, $b) {
  global $add, $mul, $fold, $n;
  return $a - $b;
}
function mul($a, $b) {
  global $add, $sub, $fold, $n;
  return $a * $b;
}
function fold($f, $xs) {
  global $add, $sub, $mul, $n;
  $r = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  $r = $f($r, $xs[$i]);
  $i = $i + 1;
};
  return $r;
}
$n = [1, 2, 3, 4, 5];
echo rtrim(json_encode(fold(function($a, $b) use (&$add, &$sub, &$mul, &$fold, &$n) {
  return add($a, $b);
}, $n), 1344)), PHP_EOL;
echo rtrim(json_encode(fold(function($a, $b) use (&$add, &$sub, &$mul, &$fold, &$n) {
  return sub($a, $b);
}, $n), 1344)), PHP_EOL;
echo rtrim(json_encode(fold(function($a, $b) use (&$add, &$sub, &$mul, &$fold, &$n) {
  return mul($a, $b);
}, $n), 1344)), PHP_EOL;
