<?php
function pow_int($base, $exp) {
  global $pow_big, $e1, $e2, $x, $s;
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
function pow_big($base, $exp) {
  global $pow_int, $e1, $e2, $x, $s;
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
$e1 = pow_int(3, 2);
$e2 = pow_int(4, $e1);
$base = 5;
$x = pow_big($base, $e2);
$s = json_encode($x, 1344);
echo "5^(4^(3^2)) has" . " " . json_encode(strlen($s), 1344) . " " . "digits:" . " " . json_encode(substr($s, 0, 20 - 0), 1344) . " " . "..." . " " . json_encode(substr($s, strlen($s) - 20, strlen($s) - strlen($s) - 20), 1344), PHP_EOL;
