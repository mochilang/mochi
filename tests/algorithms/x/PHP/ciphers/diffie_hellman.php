<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function int_to_hex($n) {
  global $seed, $PRIME, $generator, $alice_private, $alice_public, $bob_private, $bob_public, $alice_shared, $bob_shared;
  if ($n == 0) {
  return '0';
}
  $digits = '0123456789abcdef';
  $num = $n;
  $res = '';
  while ($num > 0) {
  $d = $num % 16;
  $res = substr($digits, $d, $d + 1 - $d) . $res;
  $num = _intdiv($num, 16);
};
  return $res;
}
$seed = 123456789;
function rand_int() {
  global $seed, $PRIME, $generator, $alice_private, $alice_public, $bob_private, $bob_public, $alice_shared, $bob_shared;
  $seed = (1103515245 * $seed + 12345) % 2147483648;
  return $seed;
}
$PRIME = 23;
function mod_pow($base, $exp) {
  global $seed, $PRIME, $generator, $alice_private, $alice_public, $bob_private, $bob_public, $alice_shared, $bob_shared;
  $result = 1;
  $b = $base % $PRIME;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $PRIME;
}
  $b = ($b * $b) % $PRIME;
  $e = _intdiv($e, 2);
};
  return $result;
}
function is_valid_public_key($key) {
  global $seed, $PRIME, $generator, $alice_private, $alice_public, $bob_private, $bob_public, $alice_shared, $bob_shared;
  if ($key < 2 || $key > $PRIME - 2) {
  return false;
}
  return mod_pow($key, _intdiv(($PRIME - 1), 2)) == 1;
}
function generate_private_key() {
  global $seed, $PRIME, $generator, $alice_private, $alice_public, $bob_private, $bob_public, $alice_shared, $bob_shared;
  return fmod(rand_int(), ($PRIME - 2)) + 2;
}
$generator = 5;
$alice_private = generate_private_key();
$alice_public = mod_pow($generator, $alice_private);
$bob_private = generate_private_key();
$bob_public = mod_pow($generator, $bob_private);
if (!is_valid_public_key($alice_public)) {
  $panic('Invalid public key');
}
if (!is_valid_public_key($bob_public)) {
  $panic('Invalid public key');
}
$alice_shared = mod_pow($bob_public, $alice_private);
$bob_shared = mod_pow($alice_public, $bob_private);
echo rtrim(int_to_hex($alice_shared)), PHP_EOL;
echo rtrim(int_to_hex($bob_shared)), PHP_EOL;
