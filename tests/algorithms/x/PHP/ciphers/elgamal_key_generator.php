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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$seed = 123456789;
function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483647;
  return $seed;
}
function rand_range($min, $max) {
  global $seed;
  return $min + fmod(mochi_rand(), ($max - $min + 1));
}
function mod_pow($base, $exponent, $modulus) {
  global $seed;
  $result = 1;
  $b = $base % $modulus;
  $e = $exponent;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $modulus;
}
  $e = _intdiv($e, 2);
  $b = ($b * $b) % $modulus;
};
  return $result;
}
function extended_gcd($a, $b) {
  global $seed;
  if ($b == 0) {
  return ['g' => $a, 'x' => 1, 'y' => 0];
}
  $res = extended_gcd($b, $a % $b);
  return ['g' => $res['g'], 'x' => $res['y'], 'y' => $res['x'] - (_intdiv($a, $b)) * $res['y']];
}
function mod_inverse($a, $m) {
  global $seed;
  $res = extended_gcd($a, $m);
  if ($res['g'] != 1) {
  $panic('inverse does not exist');
}
  $r = fmod($res['x'], $m);
  if ($r < 0) {
  return $r + $m;
}
  return $r;
}
function pow2($n) {
  global $seed;
  $r = 1;
  $i = 0;
  while ($i < $n) {
  $r = $r * 2;
  $i = $i + 1;
};
  return $r;
}
function is_probable_prime($n, $k) {
  global $seed;
  if ($n <= 1) {
  return false;
}
  if ($n <= 3) {
  return true;
}
  if ($n % 2 == 0) {
  return false;
}
  $r = 0;
  $d = $n - 1;
  while ($d % 2 == 0) {
  $d = _intdiv($d, 2);
  $r = $r + 1;
};
  $i = 0;
  while ($i < $k) {
  $a = rand_range(2, $n - 2);
  $x = mod_pow($a, $d, $n);
  if ($x == 1 || $x == $n - 1) {
  $i = $i + 1;
  continue;
}
  $j = 1;
  $found = false;
  while ($j < $r) {
  $x = mod_pow($x, 2, $n);
  if ($x == $n - 1) {
  $found = true;
  break;
}
  $j = $j + 1;
};
  if (!$found) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function generate_large_prime($bits) {
  global $seed;
  $min = pow2($bits - 1);
  $max = pow2($bits) - 1;
  $p = rand_range($min, $max);
  if ($p % 2 == 0) {
  $p = $p + 1;
}
  while (!is_probable_prime($p, 5)) {
  $p = $p + 2;
  if ($p > $max) {
  $p = $min + 1;
}
};
  return $p;
}
function primitive_root($p) {
  global $seed;
  while (true) {
  $g = rand_range(3, $p - 1);
  if (mod_pow($g, 2, $p) == 1) {
  continue;
}
  if (mod_pow($g, $p, $p) == 1) {
  continue;
}
  return $g;
};
}
function generate_key($key_size) {
  global $seed;
  $p = generate_large_prime($key_size);
  $e1 = primitive_root($p);
  $d = rand_range(3, $p - 1);
  $e2 = mod_inverse(mod_pow($e1, $d, $p), $p);
  $public_key = ['key_size' => $key_size, 'g' => $e1, 'e2' => $e2, 'p' => $p];
  $private_key = ['key_size' => $key_size, 'd' => $d];
  return ['public_key' => $public_key, 'private_key' => $private_key];
}
function main() {
  global $seed;
  $key_size = 16;
  $kp = generate_key($key_size);
  $pub = $kp['public_key'];
  $priv = $kp['private_key'];
  echo rtrim('public key: (' . _str($pub['key_size']) . ', ' . _str($pub['g']) . ', ' . _str($pub['e2']) . ', ' . _str($pub['p']) . ')'), PHP_EOL;
  echo rtrim('private key: (' . _str($priv['key_size']) . ', ' . _str($priv['d']) . ')'), PHP_EOL;
}
main();
