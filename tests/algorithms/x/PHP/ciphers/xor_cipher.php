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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_xor($a, $b) {
  global $ascii, $sample, $enc, $dec;
  $res = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $abit = $x % 2;
  $bbit = $y % 2;
  if ($abit != $bbit) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $res;
};
  $ascii = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  function mochi_ord($ch) {
  global $ascii, $sample, $enc, $dec;
  $i = 0;
  while ($i < strlen($ascii)) {
  if (substr($ascii, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
};
  function mochi_chr($n) {
  global $ascii, $sample, $enc, $dec;
  if ($n >= 32 && $n < 127) {
  return substr($ascii, $n - 32, $n - 31 - ($n - 32));
}
  return '';
};
  function normalize_key($key) {
  global $ascii, $sample, $enc, $dec;
  $k = $key;
  if ($k == 0) {
  $k = 1;
}
  $k = $k % 256;
  if ($k < 0) {
  $k = $k + 256;
}
  return $k;
};
  function encrypt($content, $key) {
  global $ascii, $sample, $enc, $dec;
  $k = normalize_key($key);
  $result = [];
  $i = 0;
  while ($i < strlen($content)) {
  $c = mochi_ord(substr($content, $i, $i + 1 - $i));
  $e = mochi_xor($c, $k);
  $result = _append($result, mochi_chr($e));
  $i = $i + 1;
};
  return $result;
};
  function encrypt_string($content, $key) {
  global $ascii, $sample, $enc, $dec;
  $chars = encrypt($content, $key);
  $out = '';
  foreach ($chars as $ch) {
  $out = $out . $ch;
};
  return $out;
};
  $sample = 'hallo welt';
  $enc = encrypt_string($sample, 1);
  $dec = encrypt_string($enc, 1);
  echo rtrim(_str(encrypt($sample, 1))), PHP_EOL;
  echo rtrim($enc), PHP_EOL;
  echo rtrim($dec), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
