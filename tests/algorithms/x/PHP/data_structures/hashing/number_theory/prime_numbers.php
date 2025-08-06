<?php
ini_set('memory_limit', '-1');
function isPrime($number) {
  if ($number < 2) {
  return false;
}
  if ($number < 4) {
  return true;
}
  if ($number % 2 == 0) {
  return false;
}
  $i = 3;
  while ($i * $i <= $number) {
  if ($number % $i == 0) {
  return false;
}
  $i = $i + 2;
};
  return true;
}
function nextPrime($value, $factor, $desc) {
  $v = $value * $factor;
  $firstValue = $v;
  while (!isPrime($v)) {
  if ($desc) {
  $v = $v - 1;
} else {
  $v = $v + 1;
}
};
  if ($v == $firstValue) {
  if ($desc) {
  return nextPrime($v - 1, 1, $desc);
} else {
  return nextPrime($v + 1, 1, $desc);
};
}
  return $v;
}
echo rtrim(json_encode(isPrime(0), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(1), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(2), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(3), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(27), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(87), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(563), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(2999), 1344)), PHP_EOL;
echo rtrim(json_encode(isPrime(67483), 1344)), PHP_EOL;
echo rtrim(json_encode(nextPrime(14, 1, false), 1344)), PHP_EOL;
echo rtrim(json_encode(nextPrime(14, 1, true), 1344)), PHP_EOL;
