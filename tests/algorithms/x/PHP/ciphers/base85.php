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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$ascii85_chars = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu';
function indexOf($s, $ch) {
  global $ascii85_chars;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_ord($ch) {
  global $ascii85_chars;
  $idx = _indexof($ascii85_chars, $ch);
  if ($idx >= 0) {
  return 33 + $idx;
}
  return 0;
}
function mochi_chr($n) {
  global $ascii85_chars;
  if ($n >= 33 && $n <= 117) {
  return substr($ascii85_chars, $n - 33, $n - 32 - ($n - 33));
}
  return '?';
}
function to_binary($n, $bits) {
  global $ascii85_chars;
  $b = '';
  $val = $n;
  while ($val > 0) {
  $b = _str($val % 2) . $b;
  $val = _intdiv($val, 2);
};
  while (strlen($b) < $bits) {
  $b = '0' . $b;
};
  if (strlen($b) == 0) {
  $b = '0';
}
  return $b;
}
function bin_to_int($bits) {
  global $ascii85_chars;
  $n = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  if (substr($bits, $i, $i + 1 - $i) == '1') {
  $n = $n * 2 + 1;
} else {
  $n = $n * 2;
}
  $i = $i + 1;
};
  return $n;
}
function reverse($s) {
  global $ascii85_chars;
  $res = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $res;
}
function base10_to_85($d) {
  global $ascii85_chars;
  if ($d > 0) {
  return mochi_chr($d % 85 + 33) . base10_to_85(_intdiv($d, 85));
}
  return '';
}
function base85_to_10($digits) {
  global $ascii85_chars;
  $value = 0;
  $i = 0;
  while ($i < strlen($digits)) {
  $value = $value * 85 + (mochi_ord(substr($digits, $i, $i + 1 - $i)) - 33);
  $i = $i + 1;
};
  return $value;
}
function ascii85_encode($data) {
  global $ascii85_chars;
  $binary_data = '';
  foreach (str_split($data) as $ch) {
  $binary_data = $binary_data . to_binary(mochi_ord($ch), 8);
};
  $null_values = (32 * ((strlen($binary_data) / 32) + 1) - strlen($binary_data)) / 8;
  $total_bits = 32 * ((strlen($binary_data) / 32) + 1);
  while (strlen($binary_data) < $total_bits) {
  $binary_data = $binary_data . '0';
};
  $result = '';
  $i = 0;
  while ($i < strlen($binary_data)) {
  $chunk_bits = substr($binary_data, $i, $i + 32 - $i);
  $chunk_val = bin_to_int($chunk_bits);
  $encoded = reverse(base10_to_85($chunk_val));
  $result = $result . $encoded;
  $i = $i + 32;
};
  if ($null_values % 4 != 0) {
  $result = substr($result, 0, strlen($result) - $null_values - 0);
}
  return $result;
}
function ascii85_decode($data) {
  global $ascii85_chars;
  $null_values = 5 * ((strlen($data) / 5) + 1) - strlen($data);
  $binary_data = $data;
  $i = 0;
  while ($i < $null_values) {
  $binary_data = $binary_data . 'u';
  $i = $i + 1;
};
  $result = '';
  $i = 0;
  while ($i < strlen($binary_data)) {
  $chunk = substr($binary_data, $i, $i + 5 - $i);
  $value = base85_to_10($chunk);
  $bits = to_binary($value, 32);
  $j = 0;
  while ($j < 32) {
  $byte_bits = substr($bits, $j, $j + 8 - $j);
  $c = mochi_chr(bin_to_int($byte_bits));
  $result = $result . $c;
  $j = $j + 8;
};
  $i = $i + 5;
};
  $trim = $null_values;
  if ($null_values % 5 == 0) {
  $trim = $null_values - 1;
}
  return substr($result, 0, strlen($result) - $trim - 0);
}
echo rtrim(ascii85_encode('')), PHP_EOL;
echo rtrim(ascii85_encode('12345')), PHP_EOL;
echo rtrim(ascii85_encode('base 85')), PHP_EOL;
echo rtrim(ascii85_decode('')), PHP_EOL;
echo rtrim(ascii85_decode('0etOA2#')), PHP_EOL;
echo rtrim(ascii85_decode('@UX=h+?24')), PHP_EOL;
