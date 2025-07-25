<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  global $msg, $enc, $dec;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function mochi_parseIntStr($str) {
  global $msg, $enc, $dec;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 0 + 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function mochi_ord($ch) {
  global $msg, $enc, $dec;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = _indexof($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  if ($ch >= '0' && $ch <= '9') {
  return 48 + parseIntStr($ch, 10);
}
  if ($ch == '+') {
  return 43;
}
  if ($ch == '/') {
  return 47;
}
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '=') {
  return 61;
}
  return 0;
};
  function mochi_chr($n) {
  global $msg, $enc, $dec;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  if ($n >= 48 && $n < 58) {
  $digits = '0123456789';
  return substr($digits, $n - 48, $n - 47 - ($n - 48));
}
  if ($n == 43) {
  return '+';
}
  if ($n == 47) {
  return '/';
}
  if ($n == 32) {
  return ' ';
}
  if ($n == 61) {
  return '=';
}
  return '?';
};
  function toBinary($n, $bits) {
  global $msg, $enc, $dec;
  $b = '';
  $val = $n;
  $i = 0;
  while ($i < $bits) {
  $b = _str($val % 2) . $b;
  $val = intval((_intdiv($val, 2)));
  $i = $i + 1;
};
  return $b;
};
  function binToInt($bits) {
  global $msg, $enc, $dec;
  $n = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  $n = $n * 2 + parseIntStr(substr($bits, $i, $i + 1 - $i), 10);
  $i = $i + 1;
};
  return $n;
};
  function base64Encode($text) {
  global $msg, $enc, $dec;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  $bin = '';
  foreach (str_split($text) as $ch) {
  $bin = $bin . toBinary(mochi_ord($ch), 8);
};
  while (fmod(strlen($bin), 6) != 0) {
  $bin = $bin . '0';
};
  $out = '';
  $i = 0;
  while ($i < strlen($bin)) {
  $chunk = substr($bin, $i, $i + 6 - $i);
  $val = binToInt($chunk);
  $out = $out . substr($alphabet, $val, $val + 1 - $val);
  $i = $i + 6;
};
  $pad = fmod((3 - (fmod(strlen($text), 3))), 3);
  if ($pad == 1) {
  $out = substr($out, 0, strlen($out) - 1 - 0) . '=';
}
  if ($pad == 2) {
  $out = substr($out, 0, strlen($out) - 2 - 0) . '==';
}
  return $out;
};
  function base64Decode($enc) {
  global $msg, $dec;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  $bin = '';
  $i = 0;
  while ($i < strlen($enc)) {
  $ch = substr($enc, $i, $i + 1 - $i);
  if ($ch == '=') {
  break;
}
  $idx = _indexof($alphabet, $ch);
  $bin = $bin . toBinary($idx, 6);
  $i = $i + 1;
};
  $out = '';
  $i = 0;
  while ($i + 8 <= strlen($bin)) {
  $chunk = substr($bin, $i, $i + 8 - $i);
  $val = binToInt($chunk);
  $out = $out . mochi_chr($val);
  $i = $i + 8;
};
  return $out;
};
  $msg = 'Rosetta Code Base64 decode data task';
  echo rtrim('Original : ' . $msg), PHP_EOL;
  $enc = base64Encode($msg);
  echo rtrim('\nEncoded  : ' . $enc), PHP_EOL;
  $dec = base64Decode($enc);
  echo rtrim('\nDecoded  : ' . $dec), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
