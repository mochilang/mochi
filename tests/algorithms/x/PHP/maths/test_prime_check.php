<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function is_prime($number) {
  if ($number < 0) {
  _panic('is_prime() only accepts positive integers');
}
  if ($number < 2) {
  return false;
}
  if ($number < 4) {
  return true;
}
  if ($number % 2 == 0 || $number % 3 == 0) {
  return false;
}
  $i = 5;
  while ($i * $i <= $number) {
  if ($number % $i == 0 || $number % ($i + 2) == 0) {
  return false;
}
  $i = $i + 6;
};
  return true;
}
echo rtrim(_str(is_prime(2))), PHP_EOL;
echo rtrim(_str(is_prime(3))), PHP_EOL;
echo rtrim(_str(is_prime(5))), PHP_EOL;
echo rtrim(_str(is_prime(7))), PHP_EOL;
echo rtrim(_str(is_prime(11))), PHP_EOL;
echo rtrim(_str(is_prime(13))), PHP_EOL;
echo rtrim(_str(is_prime(17))), PHP_EOL;
echo rtrim(_str(is_prime(19))), PHP_EOL;
echo rtrim(_str(is_prime(23))), PHP_EOL;
echo rtrim(_str(is_prime(29))), PHP_EOL;
echo rtrim(_str(is_prime(0))), PHP_EOL;
echo rtrim(_str(is_prime(1))), PHP_EOL;
echo rtrim(_str(is_prime(4))), PHP_EOL;
echo rtrim(_str(is_prime(6))), PHP_EOL;
echo rtrim(_str(is_prime(9))), PHP_EOL;
echo rtrim(_str(is_prime(15))), PHP_EOL;
echo rtrim(_str(is_prime(105))), PHP_EOL;
