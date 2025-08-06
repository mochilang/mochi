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
$SYMBOLS = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function gcd($a, $b) {
  global $SYMBOLS;
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $temp = $x % $y;
  $x = $y;
  $y = $temp;
};
  return $x;
}
function mod_inverse($a, $m) {
  global $SYMBOLS;
  if (gcd($a, $m) != 1) {
  $panic('mod inverse of ' . _str($a) . ' and ' . _str($m) . ' does not exist');
}
  $u1 = 1;
  $u2 = 0;
  $u3 = $a;
  $v1 = 0;
  $v2 = 1;
  $v3 = $m;
  while ($v3 != 0) {
  $q = _intdiv($u3, $v3);
  $t1 = $u1 - $q * $v1;
  $t2 = $u2 - $q * $v2;
  $t3 = $u3 - $q * $v3;
  $u1 = $v1;
  $u2 = $v2;
  $u3 = $v3;
  $v1 = $t1;
  $v2 = $t2;
  $v3 = $t3;
};
  $res = $u1 % $m;
  if ($res < 0) {
  return $res + $m;
}
  return $res;
}
function find_symbol($ch) {
  global $SYMBOLS;
  $i = 0;
  while ($i < strlen($SYMBOLS)) {
  if (substr($SYMBOLS, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function check_keys($key_a, $key_b, $mode) {
  global $SYMBOLS;
  $m = strlen($SYMBOLS);
  if ($mode == 'encrypt') {
  if ($key_a == 1) {
  $panic('The affine cipher becomes weak when key A is set to 1. Choose different key');
};
  if ($key_b == 0) {
  $panic('The affine cipher becomes weak when key B is set to 0. Choose different key');
};
}
  if ($key_a < 0 || $key_b < 0 || $key_b > $m - 1) {
  $panic('Key A must be greater than 0 and key B must be between 0 and ' . _str($m - 1));
}
  if (gcd($key_a, $m) != 1) {
  $panic('Key A ' . _str($key_a) . ' and the symbol set size ' . _str($m) . ' are not relatively prime. Choose a different key.');
}
}
function encrypt_message($key, $message) {
  global $SYMBOLS;
  $m = strlen($SYMBOLS);
  $key_a = _intdiv($key, $m);
  $key_b = $key % $m;
  check_keys($key_a, $key_b, 'encrypt');
  $cipher_text = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  $index = find_symbol($ch);
  if ($index >= 0) {
  $cipher_text = $cipher_text . substr($SYMBOLS, ($index * $key_a + $key_b) % $m, ($index * $key_a + $key_b) % $m + 1 - ($index * $key_a + $key_b) % $m);
} else {
  $cipher_text = $cipher_text . $ch;
}
  $i = $i + 1;
};
  return $cipher_text;
}
function decrypt_message($key, $message) {
  global $SYMBOLS;
  $m = strlen($SYMBOLS);
  $key_a = _intdiv($key, $m);
  $key_b = $key % $m;
  check_keys($key_a, $key_b, 'decrypt');
  $inv = mod_inverse($key_a, $m);
  $plain_text = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  $index = find_symbol($ch);
  if ($index >= 0) {
  $n = ($index - $key_b) * $inv;
  $pos = $n % $m;
  $final = ($pos < 0 ? $pos + $m : $pos);
  $plain_text = $plain_text . substr($SYMBOLS, $final, $final + 1 - $final);
} else {
  $plain_text = $plain_text . $ch;
}
  $i = $i + 1;
};
  return $plain_text;
}
function main() {
  global $SYMBOLS;
  $key = 4545;
  $msg = 'The affine cipher is a type of monoalphabetic substitution cipher.';
  $enc = encrypt_message($key, $msg);
  echo rtrim($enc), PHP_EOL;
  echo rtrim(decrypt_message($key, $enc)), PHP_EOL;
}
main();
